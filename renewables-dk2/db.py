import time
from pathlib import Path

import duckdb
import pandas as pd
import requests

START = pd.Timestamp("2024-03-01")
TODAY = pd.Timestamp.today().floor("D")

TABLES = {
    "production": "time TIMESTAMP PRIMARY KEY, onshore DOUBLE, offshore DOUBLE, solar DOUBLE",
    "energinet": "time TIMESTAMP PRIMARY KEY, onshore DOUBLE, offshore DOUBLE, solar DOUBLE",
    "weather": "time TIMESTAMP, point VARCHAR, wind_100m DOUBLE, direction_100m DOUBLE, wind_10m DOUBLE, "
               "temperature DOUBLE, radiation DOUBLE, cloud_cover DOUBLE, PRIMARY KEY (time, point)",
    "weather_runs": "run TIMESTAMP, time TIMESTAMP, point VARCHAR, wind_100m DOUBLE, direction_100m DOUBLE, "
                    "wind_10m DOUBLE, gusts DOUBLE, temperature DOUBLE, radiation DOUBLE, direct_radiation DOUBLE, "
                    "cloud_cover DOUBLE, PRIMARY KEY (run, time, point)",
    "weather_models": "time TIMESTAMP, point VARCHAR, model VARCHAR, wind_100m DOUBLE, direction_100m DOUBLE, "
                      "radiation DOUBLE, cloud_cover DOUBLE, PRIMARY KEY (time, point, model)",
}

POINTS = {
    "kriegers_flak": (55.05, 12.90),
    "rodsand": (54.55, 11.65),
    "oresund": (55.65, 12.70),
    "lolland": (54.80, 11.30),
    "falster": (54.80, 11.90),
    "south_zealand": (55.20, 11.80),
    "west_zealand": (55.50, 11.30),
    "north_zealand": (55.85, 12.30),
    "bornholm": (55.10, 14.90),
}


def connect():
    Path("data").mkdir(exist_ok=True)
    con = duckdb.connect("data/dk1.duckdb")
    for name, cols in TABLES.items():
        con.execute(f"CREATE TABLE IF NOT EXISTS {name} ({cols})")
    return con


def save(con, table, df):
    con.register("df", df)
    con.execute(f"INSERT OR REPLACE INTO {table} BY NAME SELECT * FROM df")
    con.unregister("df")


def start_from(con, table, days_back, where=""):
    # first run: everything. after that only the last few days
    last = con.execute(f"SELECT max(time) FROM {table} {where}").fetchall()[0][0]
    return START if last is None else pd.Timestamp(last).floor("D") - pd.Timedelta(days=days_back)


def get(url, params):
    # open-meteo allows 600 calls a minute and 5,000 an hour, so a long download waits when it hits the limit
    for _ in range(150):
        try:
            r = requests.get(url, params=params, timeout=300)
        except requests.ConnectionError:
            time.sleep(30)
            continue
        if r.status_code == 429:
            time.sleep(60)
            continue
        if r.status_code == 400:
            raise RuntimeError(r.text)
        r.raise_for_status()
        try:
            return r.json()
        except ValueError:  # an empty or cut-off answer, try again
            time.sleep(30)
    raise RuntimeError(f"no answer from {url}")
