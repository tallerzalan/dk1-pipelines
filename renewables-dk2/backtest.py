# backtest from march 2025: a power curve, lightgbm and their average, refit on the first of each month
# rules in the readme, set before this was run

from pathlib import Path

import duckdb
import lightgbm as lgb
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.api as sm

TARGETS = ["onshore", "offshore", "solar"]
LAND = ["lolland", "falster", "south_zealand", "west_zealand", "north_zealand", "bornholm"]
SEA = ["kriegers_flak", "rodsand", "oresund"]
TEST_START = pd.Timestamp("2025-03-01")
DELAY = 10  # days before settled output is known
COLOR = {"power curve": "#eb6834", "lightgbm": "#2a78d6", "average": "#1baf7a", "energinet": "#8a8984"}

con = duckdb.connect("data/dk1.duckdb", read_only=True)
prod = con.execute("SELECT * FROM production ORDER BY time").df().set_index("time")
energinet = con.execute("SELECT * FROM energinet").df().set_index("time")
w = con.execute("SELECT * FROM weather").df()
con.close()

# one row per hour, one column per variable and point
angle = np.radians(w.pop("direction_100m"))
w["dir_sin"], w["dir_cos"] = np.sin(angle), np.cos(angle)
X = w.pivot(index="time", columns="point")
X.columns = [f"{v}_{p}" for v, p in X.columns]
X["wind_land"] = X[[f"wind_100m_{p}" for p in LAND]].mean(axis=1)
X["wind_sea"] = X[[f"wind_100m_{p}" for p in SEA]].mean(axis=1)
X["sun"] = X[[f"radiation_{p}" for p in LAND]].mean(axis=1)
local = X.index.tz_localize("UTC").tz_convert("Europe/Copenhagen")
X["hour"] = local.hour
X["day_of_year"] = local.dayofyear
X["date"] = pd.to_datetime(local.date)
X = X.dropna(subset=["wind_land", "wind_sea", "sun"])
CURVE = {"onshore": ("wind_land", 0.5), "offshore": ("wind_sea", 0.5), "solar": ("sun", 25)}
FEATURES = [c for c in X.columns if c != "date"]


def fleet(y):
    # highest hourly output in the 60 days up to DELAY days before each date
    daily = y.groupby(pd.to_datetime(y.index.tz_localize("UTC").tz_convert("Europe/Copenhagen").date)).max()
    return daily.rolling("60D").max().shift(DELAY, freq="D")


def power_curve(train, test, x, step):
    b = (train[x] / step).round() * step
    curve = train.groupby(b)["share"].mean()
    return np.interp(test[x], curve.index, curve.values)


def fit_lgb(train):
    m = lgb.LGBMRegressor(objective="l1", n_estimators=600, learning_rate=0.05, num_leaves=31,
                          min_child_samples=50, subsample=0.8, subsample_freq=1, colsample_bytree=0.8,
                          verbose=-1, random_state=1)
    return m.fit(train[FEATURES], train["share"])


rows = []
for t in TARGETS:
    cap = fleet(prod[t])
    d = X.join(prod[t].rename("actual"), how="inner")
    d["cap"] = d["date"].map(cap)
    d = d.dropna(subset=["actual", "cap"])
    d["share"] = d["actual"] / d["cap"]
    for month in pd.date_range(TEST_START, d["date"].max(), freq="MS"):
        train = d[d["date"] <= month - pd.Timedelta(days=DELAY)]
        test = d[(d["date"] >= month) & (d["date"] < month + pd.offsets.MonthBegin())]
        if test.empty:
            continue
        x, step = CURVE[t]
        pc = np.clip(power_curve(train, test, x, step), 0, 1) * test["cap"]
        gb = np.clip(fit_lgb(train).predict(test[FEATURES]), 0, 1) * test["cap"]
        rows.append(pd.DataFrame({"target": t, "actual": test["actual"], "cap": test["cap"], "power curve": pc,
                                  "lightgbm": gb, "average": (pc + gb) / 2, "date": test["date"]}))
        print(t, f"{month:%Y-%m}", len(train), "hours to train on")

fc = pd.concat(rows)
fc["energinet"] = [energinet[t].get(i, np.nan) for t, i in zip(fc["target"], fc.index)]
fc.index.name = "time"

MODELS = ["power curve", "lightgbm", "average", "energinet"]
scores, tests = [], []
for t, g in fc.groupby("target", sort=False):
    g = g.dropna(subset=MODELS)
    for m in MODELS:
        mae = (g[m] - g["actual"]).abs().mean()
        scores.append({"target": t, "model": m, "mae_mw": mae, "mae_share_of_fleet": mae / g["cap"].mean(),
                       "bias_mw": (g[m] - g["actual"]).mean(), "hours": len(g)})
    daily = (g[["power curve", "lightgbm"]].sub(g["actual"], axis=0).abs()).groupby(g["date"]).mean()
    diff = daily["power curve"] - daily["lightgbm"]
    ols = sm.OLS(diff.values, np.ones(len(diff))).fit(cov_type="HAC", cov_kwds={"maxlags": 7})
    tests.append({"target": t, "days": len(diff), "mean_gain_mw": diff.mean(), "t": ols.tvalues[0],
                  "p_one_sided": ols.pvalues[0] / 2 if ols.tvalues[0] > 0 else 1 - ols.pvalues[0] / 2})

Path("results").mkdir(exist_ok=True)
fc.drop(columns="date").round(1).to_csv("results/backtest_forecasts.csv")
pd.DataFrame(scores).round(4).to_csv("results/backtest_scores.csv", index=False)
pd.DataFrame(tests).round(4).to_csv("results/backtest_tests.csv", index=False)
print(pd.DataFrame(scores).pivot(index="model", columns="target", values="mae_mw").reindex(MODELS)[TARGETS].round(0))
print(pd.DataFrame(tests).round(4).to_string(index=False))

# monthly mean absolute error, as a share of the fleet
Path("charts").mkdir(exist_ok=True)
fig, axes = plt.subplots(1, 3, figsize=(12, 3.6), sharey=True)
for ax, t in zip(axes, TARGETS):
    g = fc[fc["target"] == t].dropna(subset=MODELS)
    month = g["date"].dt.to_period("M").dt.to_timestamp()
    for m in MODELS:
        e = (g[m] - g["actual"]).abs().groupby(month).mean() / g["cap"].groupby(month).mean()
        ax.plot(e.index, 100 * e.values, color=COLOR[m], lw=2, ls="--" if m == "energinet" else "-", label=m)
    ax.set_title(f"{t.capitalize()}: monthly error", loc="left", fontsize=11)
    ax.xaxis.set_major_locator(mdates.MonthLocator((1, 4, 7, 10)))
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%b %y"))
    ax.spines[["top", "right"]].set_visible(False)
    ax.grid(color="#e4e3df", lw=0.6)
axes[0].set_ylabel("mean absolute error, % of fleet")
axes[0].legend(frameon=False)
fig.tight_layout()
fig.savefig("charts/backtest.png", dpi=150, bbox_inches="tight")
