# weather forecasts over dk1's wind farms and solar areas, from open-meteo's archive of old forecast runs
# previous_day2 is the run from two days before each hour, so it was out before the auction the day before

import pandas as pd

from db import POINTS, TODAY, connect, get, save, start_from

URL = "https://previous-runs-api.open-meteo.com/v1/forecast"
VARIABLES = {
    "wind_speed_100m": "wind_100m",
    "wind_direction_100m": "direction_100m",
    "wind_speed_10m": "wind_10m",
    "temperature_2m": "temperature",
    "shortwave_radiation": "radiation",
    "cloud_cover": "cloud_cover",
}

con = connect()
for point, (lat, lon) in POINTS.items():
    start = start_from(con, "weather", 3, f"WHERE point = '{point}'")
    js = get(URL, {"latitude": lat, "longitude": lon, "start_date": f"{start:%Y-%m-%d}",
                   "end_date": f"{TODAY + pd.Timedelta(days=1):%Y-%m-%d}", "timezone": "GMT",
                   "wind_speed_unit": "ms", "hourly": ",".join(v + "_previous_day2" for v in VARIABLES)})
    w = pd.DataFrame(js["hourly"]).rename(columns=lambda c: VARIABLES.get(c.replace("_previous_day2", ""), c))
    w["time"] = pd.to_datetime(w["time"])
    w = w.dropna(subset=list(VARIABLES.values()), how="all").assign(point=point)
    save(con, "weather", w)
    print(f"{point}: {len(w)} hours from {start:%Y-%m-%d}")

print(con.execute("SELECT point, count(*) AS hours, max(time) AS last, round(avg(wind_100m), 1) AS wind, "
                  "round(avg(radiation)) AS radiation FROM weather GROUP BY point ORDER BY point").df().to_string(index=False))
con.close()
