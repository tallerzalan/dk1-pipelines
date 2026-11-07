# a first look at the data: how output follows the weather, and how the offshore fleet has grown

from pathlib import Path

import duckdb
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

ONSHORE = ["west_jutland", "north_jutland", "mid_jutland", "south_jutland", "east_jutland", "funen"]
OFFSHORE = ["horns_rev", "thor", "anholt"]
COLOR = {"onshore": "#2a78d6", "offshore": "#eb6834", "solar": "#1baf7a", "energinet": "#8a8984"}
INK, MUTED, GRID = "#0b0b0b", "#52514e", "#e4e3df"

plt.rcParams.update({"font.size": 10, "axes.edgecolor": MUTED, "axes.labelcolor": MUTED, "xtick.color": MUTED,
                     "ytick.color": MUTED, "axes.spines.top": False, "axes.spines.right": False,
                     "axes.titlelocation": "left", "axes.titlesize": 11, "axes.titlecolor": INK,
                     "axes.grid": True, "grid.color": GRID, "grid.linewidth": 0.6, "figure.dpi": 150})

con = duckdb.connect("data/dk1.duckdb", read_only=True)
prod = con.execute("SELECT * FROM production").df().set_index("time")
fc = con.execute("SELECT * FROM energinet").df().set_index("time")
w = con.execute("SELECT * FROM weather").df()
con.close()

wind_on = w[w["point"].isin(ONSHORE)].groupby("time")["wind_100m"].mean()
wind_off = w[w["point"].isin(OFFSHORE)].groupby("time")["wind_100m"].mean()
sun = w[w["point"].isin(ONSHORE)].groupby("time")["radiation"].mean()
d = prod.join(pd.DataFrame({"wind_on": wind_on, "wind_off": wind_off, "sun": sun}), how="inner").dropna()
Path("charts").mkdir(exist_ok=True)

# output against the forecast weather, with the median in each bin
fig, axes = plt.subplots(1, 3, figsize=(12, 3.8))
for ax, (x, y, label, step) in zip(axes, [("wind_on", "onshore", "wind speed at 100 m, m/s", 0.5),
                                         ("wind_off", "offshore", "wind speed at 100 m, m/s", 0.5),
                                         ("sun", "solar", "solar radiation, W/m²", 25)]):
    ax.scatter(d[x], d[y], s=2, alpha=0.08, color=COLOR[y], linewidths=0, rasterized=True)
    bins = (d[x] / step).round() * step
    med = d.groupby(bins)[y].median()
    med = med[d.groupby(bins)[y].size() >= 30]
    ax.plot(med.index, med.values, color=INK, lw=2)
    ax.set_title(f"{y.capitalize()}: MW against forecast weather")
    ax.set_xlabel(label)
axes[0].set_ylabel("MW")
fig.text(0.01, -0.03, "Each dot is an hour, Mar 2024 to Oct 2026. Weather: Open-Meteo forecasts made two days before, "
         "averaged over the points. Black line: the median in each bin.", color=MUTED, fontsize=8.5)
fig.tight_layout()
fig.savefig("charts/output_vs_weather.png", bbox_inches="tight")

# monthly average: actual output and energinet's day-ahead forecast
m = prod.join(fc, rsuffix="_energinet").resample("MS").mean()
m = m[prod.resample("MS").size() > 600]
fig, axes = plt.subplots(1, 3, figsize=(12, 3.6), sharey=True)
for ax, t in zip(axes, ["onshore", "offshore", "solar"]):
    ax.plot(m.index, m[t], color=COLOR[t], lw=2, label="actual")
    ax.plot(m.index, m[f"{t}_energinet"], color=COLOR["energinet"], lw=2, ls="--", label="Energinet's forecast")
    ax.set_title(f"{t.capitalize()}, monthly average")
    ax.xaxis.set_major_locator(mdates.MonthLocator((1, 7)))
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%b %y"))
axes[0].set_ylabel("MW")
axes[0].legend(frameon=False, loc="upper left")
fig.text(0.01, -0.03, "Energinet's DK1 day-ahead forecast, published after the noon auction, against settled output.",
         color=MUTED, fontsize=8.5)
fig.tight_layout()
fig.savefig("charts/actual_vs_energinet.png", bbox_inches="tight")

# average day by season
season = np.where(d.index.month.isin([4, 5, 6, 7, 8, 9]), "Apr-Sep", "Oct-Mar")
hour = d.index.tz_localize("UTC").tz_convert("Europe/Copenhagen").hour
prof = d[["onshore", "offshore", "solar"]].groupby([season, hour]).mean()
fig, axes = plt.subplots(1, 2, figsize=(9, 3.4), sharey=True)
for ax, s in zip(axes, ["Oct-Mar", "Apr-Sep"]):
    ends = prof.loc[s].iloc[-1].sort_values()
    y = ends.values.copy()
    for i in range(1, len(y)):  # keep the end labels apart
        y[i] = max(y[i], y[i - 1] + 70)
    for t, yl in zip(ends.index, y):
        ax.plot(prof.loc[s].index, prof.loc[s][t], color=COLOR[t], lw=2)
        ax.text(23.3, yl, t, color=MUTED, fontsize=8.5, va="center")
    ax.set_title(f"Average day, {s}")
    ax.set_xlabel("hour, Danish time")
    ax.set_xticks([0, 6, 12, 18, 23])
    ax.set_xlim(0, 26)
axes[0].set_ylabel("MW")
fig.tight_layout()
fig.savefig("charts/average_day.png", bbox_inches="tight")

print("charts saved: output_vs_weather.png, actual_vs_energinet.png, average_day.png")
print(d[["onshore", "offshore", "solar"]].corrwith(d[["wind_on", "wind_off", "sun"]].set_axis(
    ["onshore", "offshore", "solar"], axis=1)).round(3).rename("correlation with the weather").to_string())
