# the same weather from four other models, made two days before each hour, as extra inputs
# a model open-meteo doesn't have, or doesn't archive, is skipped with its reason

import pandas as pd

from db import POINTS, TODAY, connect, get, save, start_from

URL = "https://previous-runs-api.open-meteo.com/v1/forecast"
MODELS = ["icon_eu", "dmi_harmonie_arome_europe", "metno_nordic", "gfs_seamless"]
VARIABLES = {
    "wind_speed_100m": "wind_100m",
    "wind_direction_100m": "direction_100m",
    "shortwave_radiation": "radiation",
    "cloud_cover": "cloud_cover",
}

con = connect()
for model in MODELS:
    for point, (lat, lon) in POINTS.items():
        start = start_from(con, "weather_models", 3, f"WHERE point = '{point}' AND model = '{model}'")
        try:
            js = get(URL, {"latitude": lat, "longitude": lon, "models": model, "start_date": f"{start:%Y-%m-%d}",
                           "end_date": f"{TODAY + pd.Timedelta(days=1):%Y-%m-%d}", "timezone": "GMT",
                           "wind_speed_unit": "ms", "hourly": ",".join(v + "_previous_day2" for v in VARIABLES)})
        except RuntimeError as e:
            print(f"{model}: skipped, {e}")
            with open("data/skipped.txt", "a") as f:
                f.write(f"{pd.Timestamp.now():%Y-%m-%d %H:%M} weather_models {model}: {e}\n")
            break
        w = pd.DataFrame(js["hourly"]).rename(columns=lambda c: VARIABLES.get(c.replace("_previous_day2", ""), c))
        w["time"] = pd.to_datetime(w["time"])
        w = w.dropna(subset=list(VARIABLES.values()), how="all").assign(point=point, model=model)
        save(con, "weather_models", w)
        print(f"{model} {point}: {len(w)} hours from {start:%Y-%m-%d}")

print(con.execute("SELECT model, count(*) AS rows, min(time) AS first, max(time) AS last, "
                  "round(avg(wind_100m), 1) AS wind, round(avg(radiation)) AS radiation "
                  "FROM weather_models GROUP BY model ORDER BY model").df().to_string(index=False))
con.close()
