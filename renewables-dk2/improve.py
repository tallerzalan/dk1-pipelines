# trying to beat energinet, by the rules in the readme
#   python improve.py        tries each addition on march-december 2025 and saves what is kept
#   python improve.py final  runs the kept model once on 2026 and compares it with energinet

import json
import sys
from pathlib import Path

import duckdb
import lightgbm as lgb
import numpy as np
import pandas as pd
import statsmodels.api as sm

TARGETS = ["onshore", "offshore", "solar"]
LAND = ["lolland", "falster", "south_zealand", "west_zealand", "north_zealand", "bornholm"]
SEA = ["kriegers_flak", "rodsand", "oresund"]
TUNE = pd.date_range("2025-03-01", "2025-12-01", freq="MS")
FINAL_START = pd.Timestamp("2026-01-01")
DELAY = 10
PARAMS = dict(objective="l1", n_estimators=600, learning_rate=0.05, num_leaves=31, min_child_samples=50,
              subsample=0.8, subsample_freq=1, colsample_bytree=0.8, verbose=-1, random_state=1)
PARAM_TRIES = {"fewer leaves": dict(num_leaves=15), "more leaves": dict(num_leaves=63, min_child_samples=100),
               "slower": dict(learning_rate=0.025, n_estimators=1200)}
Path("results").mkdir(exist_ok=True)

con = duckdb.connect("data/dk1.duckdb", read_only=True)
prod = con.execute("SELECT * FROM production ORDER BY time").df().set_index("time")
energinet = con.execute("SELECT * FROM energinet").df().set_index("time")
base = con.execute("SELECT * FROM weather").df()
runs = con.execute("SELECT * FROM weather_runs").df()
models = con.execute("SELECT * FROM weather_models").df()
con.close()
if runs.empty or models.empty:
    sys.exit("run get_weather_runs.py and get_weather_models.py first")


def local_date(t):
    return pd.to_datetime(t.tz_localize("UTC").tz_convert("Europe/Copenhagen").date)


def wide(w, prefix):
    w = w.copy()
    angle = np.radians(w.pop("direction_100m"))
    w["dir_sin"], w["dir_cos"] = np.sin(angle), np.cos(angle)
    x = w.pivot(index="time", columns="point")
    x.columns = [f"{prefix}{v}_{p}" for v, p in x.columns]
    for name, pts in [("land", LAND), ("sea", SEA)]:
        x[f"{prefix}wind_{name}"] = x[[f"{prefix}wind_100m_{p}" for p in pts]].mean(axis=1)
        x[f"{prefix}wind_{name}_sd"] = x[[f"{prefix}wind_100m_{p}" for p in pts]].std(axis=1)
    x[f"{prefix}sun"] = x[[f"{prefix}radiation_{p}" for p in LAND]].mean(axis=1)
    return x


# weather made two days before (base), and ecmwf's 00 utc run from the day before each danish day
B = wide(base, "")
runs = runs[runs["run"] == local_date(pd.DatetimeIndex(runs["time"])) - pd.Timedelta(days=1)].drop(columns="run")
E = wide(runs, "ec_")

# other models, averaged over land and sea
mod = []
for m, g in models.groupby("model"):
    land = g[g["point"].isin(LAND)].groupby("time")
    mod.append(pd.DataFrame({f"{m}_wind_land": land["wind_100m"].mean(), f"{m}_sun": land["radiation"].mean(),
                             f"{m}_cloud": land["cloud_cover"].mean(),
                             f"{m}_wind_sea": g[g["point"].isin(SEA)].groupby("time")["wind_100m"].mean()}))
M = pd.concat(mod, axis=1)

X = B.join(E, how="left").join(M, how="left")
local = X.index.tz_localize("UTC").tz_convert("Europe/Copenhagen")
X["hour"], X["day_of_year"], X["date"] = local.hour, local.dayofyear, local_date(X.index)

# the sun's height mid-hour over zealand, and how clear the sky is forecast to be
mid = X.index - pd.Timedelta(minutes=30)
decl = np.radians(23.44) * np.sin(2 * np.pi * (284 + mid.dayofyear) / 365)
angle = np.radians(15 * (mid.hour + mid.minute / 60 + 11.8 / 15 - 12))
lat = np.radians(55.4)
sin_h = np.sin(lat) * np.sin(decl) + np.cos(lat) * np.cos(decl) * np.cos(angle)
X["sun_height"] = np.degrees(np.arcsin(sin_h))
clear = np.where(sin_h > 0.05, 1098 * sin_h * np.exp(-0.057 / np.clip(sin_h, 0.05, None)), np.nan)
for p in ["", "ec_"]:
    X[f"{p}clear_sky"] = np.clip(X[f"{p}sun"] / clear, 0, 1.5)

# for solar: where the sun is, and the radiation on a panel tilted 35 degrees to the south
cos_az = (np.sin(decl) - sin_h * np.sin(lat)) / (np.sqrt(1 - sin_h ** 2) * np.cos(lat))
azimuth = np.degrees(np.arccos(np.clip(cos_az, -1, 1)))
X["sun_azimuth"] = np.where(angle > 0, 360 - azimuth, azimuth)
X["ec_direct"] = X[[f"ec_direct_radiation_{p}" for p in LAND]].mean(axis=1)
X["ec_diffuse"] = (X["ec_sun"] - X["ec_direct"]).clip(lower=0)
tilt = np.radians(35)
cos_in = sin_h * np.cos(tilt) + np.sqrt(1 - sin_h ** 2) * np.sin(tilt) * np.cos(np.radians(X["sun_azimuth"] - 180))
beam = np.where(sin_h > 0.05, (X["ec_direct"] / np.clip(sin_h, 0.05, None)).clip(upper=1100), 0)
X["ec_panel"] = beam * np.clip(cos_in, 0, None) + X["ec_diffuse"] * (1 + np.cos(tilt)) / 2
SOLAR = ["sun_height", "sun_azimuth", "ec_clear_sky", "ec_direct", "ec_diffuse", "ec_panel"]

CALENDAR = ["hour", "day_of_year"]


def columns(cfg):
    p, src = ("ec_", E.columns) if cfg["weather"] == "ecmwf" else ("", B.columns)
    cols = [c for c in src if cfg["extras"] or c not in {p + "wind_land_sd", p + "wind_sea_sd"}]
    if cfg["extras"]:
        cols += [p + "clear_sky", "sun_height"]
    if cfg["models"]:
        cols += list(M.columns)
    if cfg.get("solar"):
        cols += [c for c in SOLAR if c not in cols]
    return cols + CALENDAR


def fleet(y, days=60):
    daily = y.groupby(local_date(y.index)).max()
    return daily.rolling(f"{days}D").max().shift(DELAY, freq="D")


def backtest(t, cfg, months):
    cap = fleet(prod[t], cfg.get("fleet_days", 60))
    d = X.join(prod[t].rename("actual"), how="inner")
    d["cap"] = d["date"].map(cap)
    d = d.dropna(subset=["actual", "cap", "wind_land"])
    d["share"] = d["actual"] / d["cap"]
    cols = columns(cfg)
    out = []
    for month in months:
        train = d[d["date"] <= month - pd.Timedelta(days=DELAY)]
        test = d[(d["date"] >= month) & (d["date"] < month + pd.offsets.MonthBegin())]
        if test.empty:
            continue
        m = lgb.LGBMRegressor(**{**PARAMS, **PARAM_TRIES.get(cfg["params"], {})})
        m.fit(train[cols], train["share"])
        out.append(pd.DataFrame({"actual": test["actual"], "cap": test["cap"], "date": test["date"],
                                 "forecast": np.clip(m.predict(test[cols]), 0, 1) * test["cap"]}))
    return pd.concat(out)


def mae(f):
    return (f["forecast"] - f["actual"]).abs().mean()


def dm(ours, other, actual, dates):
    # one-sided: are our daily errors lower
    diff = ((other - actual).abs() - (ours - actual).abs()).groupby(dates).mean()
    fit = sm.OLS(diff.values, np.ones(len(diff))).fit(cov_type="HAC", cov_kwds={"maxlags": 7})
    t = fit.tvalues[0]
    return fit.pvalues[0] / 2 if t > 0 else 1 - fit.pvalues[0] / 2


RUN = __name__ == "__main__"
if RUN and len(sys.argv) == 1:
    log, chosen = [], {}
    for t in TARGETS:
        cfg = {"weather": "base", "models": False, "extras": False, "params": "base"}
        best = mae(backtest(t, cfg, TUNE))
        log.append({"target": t, "try": "start", "mae": best, "kept": True})
        print(f"{t} start: {best:.1f}")
        tries = [("ecmwf 00 utc run", {"weather": "ecmwf"}), ("other models", {"models": True}),
                 ("more features", {"extras": True})] + [(k, {"params": k}) for k in PARAM_TRIES]
        for name, change in tries:
            score = mae(backtest(t, {**cfg, **change}, TUNE))
            kept = score < best
            log.append({"target": t, "try": name, "mae": score, "kept": kept})
            print(f"{t} {name}: {score:.1f} {'kept' if kept else ''}")
            if kept:
                cfg, best = {**cfg, **change}, score
        chosen[t] = cfg
    pd.DataFrame(log).round(2).to_csv("results/improve_tuning.csv", index=False)
    Path("results/improve_choice.json").write_text(json.dumps(chosen, indent=2))
    print(json.dumps(chosen, indent=2))

elif RUN and sys.argv[1] == "final":
    if Path("results/final_scores.csv").exists() and "again" not in sys.argv:
        sys.exit("the final run is done once. results/final_scores.csv has it")
    chosen = json.loads(Path("results/improve_choice.json").read_text())
    rows, forecasts = [], []
    for t in TARGETS:
        f = backtest(t, chosen[t], pd.date_range(FINAL_START, X["date"].max(), freq="MS"))
        f["energinet"] = energinet[t].reindex(f.index)
        both = pd.concat([prod[t].rename("actual"), energinet[t].rename("energinet")], axis=1, sort=True).dropna()
        daily = both.groupby(local_date(both.index)).sum()
        ratio = (daily["actual"].rolling("60D").sum() / daily["energinet"].rolling("60D").sum()).shift(DELAY, freq="D")
        f["energinet_scaled"] = f["energinet"] * f["date"].map(ratio)
        f = f.dropna()
        row = {"target": t, "hours": len(f), "ours": mae(f), "energinet": (f["energinet"] - f["actual"]).abs().mean(),
               "energinet_scaled": (f["energinet_scaled"] - f["actual"]).abs().mean(),
               "p_vs_energinet": dm(f["forecast"], f["energinet"], f["actual"], f["date"]),
               "p_vs_scaled": dm(f["forecast"], f["energinet_scaled"], f["actual"], f["date"])}
        row["beaten"] = bool(row["ours"] < min(row["energinet"], row["energinet_scaled"])
                             and max(row["p_vs_energinet"], row["p_vs_scaled"]) < 0.05)
        rows.append(row)
        forecasts.append(f.assign(target=t))
    pd.concat(forecasts).drop(columns="date").round(1).to_csv("results/final_forecasts.csv", index_label="time")
    pd.DataFrame(rows).round(4).to_csv("results/final_scores.csv", index=False)
    print(pd.DataFrame(rows).round(3).to_string(index=False))
