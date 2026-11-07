# ecmwf's 00 utc run from the day before each day, from open-meteo's archive of single runs (from 14 march 2024)
# the run is ready around 07 utc, before a forecast made at 10:00 danish time. one request per run, all points at once
# about 8,600 calls to open-meteo in all, more than its hourly limit, so it waits when needed. it can be stopped
# and started again: runs already saved are skipped

import pandas as pd

from db import POINTS, TODAY, connect, get, save

URL = "https://single-runs-api.open-meteo.com/v1/forecast"
MODEL = "ecmwf_ifs"
FIRST_RUN = pd.Timestamp("2024-03-14")
VARIABLES = {
    "wind_speed_100m": "wind_100m",
    "wind_direction_100m": "direction_100m",
    "wind_speed_10m": "wind_10m",
    "wind_gusts_10m": "gusts",
    "temperature_2m": "temperature",
    "shortwave_radiation": "radiation",
    "direct_radiation": "direct_radiation",
    "cloud_cover": "cloud_cover",
}

con = connect()
done = set(con.execute("SELECT DISTINCT run FROM weather_runs").df()["run"])
runs = [r for r in pd.date_range(FIRST_RUN, TODAY, freq="D") if r not in done]
print(f"{len(runs)} runs to download")

lats = ",".join(str(lat) for lat, _ in POINTS.values())
lons = ",".join(str(lon) for _, lon in POINTS.values())
for i, run in enumerate(runs):
    try:
        js = get(URL, {"latitude": lats, "longitude": lons, "models": MODEL, "run": f"{run:%Y-%m-%dT%H:%M}",
                       "forecast_days": 3, "timezone": "GMT", "wind_speed_unit": "ms",
                       "hourly": ",".join(VARIABLES)})
        parts = []
        for point, loc in zip(POINTS, js if isinstance(js, list) else [js]):
            w = pd.DataFrame(loc["hourly"]).rename(columns=VARIABLES)
            w["time"] = pd.to_datetime(w["time"])
            parts.append(w.assign(point=point, run=run))
        w = pd.concat(parts)
    except Exception as e:  # a run missing from the archive is skipped and written down
        print(f"{run:%Y-%m-%d}: skipped, {e}")
        with open("data/skipped.txt", "a") as f:
            f.write(f"{pd.Timestamp.now():%Y-%m-%d %H:%M} weather_runs {run:%Y-%m-%d}: {e}\n")
        continue
    w = w[(w["time"] >= run + pd.Timedelta(hours=18)) & (w["time"] < run + pd.Timedelta(hours=54))]
    save(con, "weather_runs", w.dropna(subset=list(VARIABLES.values()), how="all"))
    if i % 25 == 0:
        print(f"{run:%Y-%m-%d}: {len(w)} rows ({i + 1} of {len(runs)})")

print(con.execute("SELECT count(DISTINCT run) AS runs, min(run) AS first, max(run) AS last, "
                  "round(avg(wind_100m), 1) AS wind, round(avg(gusts), 1) AS gusts, "
                  "round(avg(direct_radiation)) AS direct FROM weather_runs").df().to_string(index=False))
con.close()
