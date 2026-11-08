"""Downloads from Energi Data Service and Open-Meteo, cached as Parquet so a rerun only fetches what is missing."""
import json
import time
from pathlib import Path

import pandas as pd
import requests

from .zones import GERMANY, Site, Zone

EDS = "https://api.energidataservice.dk/dataset/"
PREVIOUS_RUNS = "https://previous-runs-api.open-meteo.com/v1/forecast"
SWITCH = "2025-10-01"  # day-ahead prices are per quarter-hour from here
WEATHER_VARIABLE = {"wind": "wind_speed_100m_previous_day2", "solar": "shortwave_radiation_previous_day2"}


def get_json(url: str, params: dict, tries: int = 8) -> dict:
    """Both services are sometimes slow or rate limited, so wait and retry rather than fail a long run."""
    for attempt in range(tries):
        try:
            response = requests.get(url, params=params, timeout=300)
            if response.status_code != 429 and response.status_code < 500:
                response.raise_for_status()
                return response.json()
        except (requests.ConnectionError, requests.Timeout) as err:
            print(f"  {url}: {type(err).__name__}, try {attempt + 1}", flush=True)
        else:
            print(f"  {url}: status {response.status_code}, try {attempt + 1}", flush=True)
        time.sleep(min(15 * 2 ** attempt, 240))
    raise RuntimeError(f"no answer from {url} after {tries} tries")


def eds(dataset: str, start: str, end: str, area: str) -> pd.DataFrame:
    body = get_json(EDS + dataset, {"start": start, "end": end, "limit": 0, "filter": json.dumps({"PriceArea": [area]})})
    return pd.DataFrame(body["records"])


def area_prices(area: str, start: str, end: str) -> pd.Series:
    """Hourly day-ahead prices with a UTC index. Quarter-hours are averaged, and an hour with a missing quarter is dropped."""
    old = eds("Elspotprices", start, SWITCH, area)
    new = eds("DayAheadPrices", SWITCH, end, area)
    hourly = pd.Series(old["SpotPriceEUR"].to_numpy(dtype=float), index=pd.to_datetime(old["HourUTC"], utc=True))
    quarters = pd.Series(new["DayAheadPriceEUR"].to_numpy(dtype=float), index=pd.to_datetime(new["TimeUTC"], utc=True))
    grouped = quarters.groupby(quarters.index.floor("h"))
    combined = pd.concat([hourly, grouped.mean()[grouped.count() == 4]]).dropna()
    return combined[~combined.index.duplicated()].sort_index()


def site_weather(site: Site, start: str, end: str) -> pd.Series:
    """Forecast made 48 hours ahead for one site, fetched a quarter at a time."""
    parts = []
    for first in pd.date_range(start, end, freq="QS").union([pd.Timestamp(start)]):
        last = min(first + pd.offsets.QuarterEnd(0), pd.Timestamp(end))
        variable = WEATHER_VARIABLE[site.kind]
        hourly = get_json(PREVIOUS_RUNS, {"latitude": site.lat, "longitude": site.lon, "hourly": variable, "timezone": "GMT",
                                          "start_date": f"{first:%Y-%m-%d}", "end_date": f"{last:%Y-%m-%d}"})["hourly"]
        parts.append(pd.Series(hourly[variable], index=pd.to_datetime(hourly["time"], utc=True), dtype=float))
        print(f"  weather {site.name} {first:%Y-%m}", flush=True)
        time.sleep(0.5)
    return pd.concat(parts).rename(f"{site.kind}_{site.name}").sort_index()


def weather(sites: tuple[Site, ...], start: str, end: str) -> pd.DataFrame:
    return pd.concat([site_weather(site, start, end) for site in sites], axis=1)


class MarketData:
    """All the inputs for one zone."""

    def __init__(self, zone: Zone, cache: Path, end: str | None = None):
        self.zone, self.cache = zone, Path(cache) / zone.code
        self.end = end or f"{pd.Timestamp.now(tz='UTC') + pd.Timedelta(days=2):%Y-%m-%d}"
        self.cache.mkdir(parents=True, exist_ok=True)

    def _cached(self, name: str, fetch) -> pd.DataFrame:
        path = self.cache / f"{name}.parquet"
        if path.exists():
            return pd.read_parquet(path)
        frame = fetch()
        frame.to_parquet(path)
        return frame

    # the models train on two years before each day, and the backtest starts on 1 oct 2025
    HISTORY_FROM = "2023-06-01"

    def prices(self) -> pd.Series:
        return self._cached("prices", lambda: area_prices(self.zone.code, self.HISTORY_FROM, self.end).to_frame("price"))["price"]

    def neighbours(self) -> pd.DataFrame:
        return self._cached("neighbours", lambda: pd.concat(
            {a: area_prices(a, self.HISTORY_FROM, self.end) for a in self.zone.neighbours}, axis=1))

    def weather(self) -> pd.DataFrame:
        return self._cached("weather", lambda: weather(self.zone.sites, "2024-01-01", self.end))

    def weather_germany(self) -> pd.DataFrame:
        return self._cached("weather_germany", lambda: weather(GERMANY, "2024-03-01", self.end))
