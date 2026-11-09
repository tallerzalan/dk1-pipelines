"""The daily run: bring the rolling input files up to date, then add tomorrow's 24 prices to the forecast log before the
noon auction. The files keep the layout the R pipeline used, so either version can read the other's."""
import math
import os
from datetime import date, datetime, timedelta, timezone
from pathlib import Path

import numpy as np
import pandas as pd

from .data import area_prices, weather
from .features import GROUPS, TZ, Features
from .models import BmtX, LearX, XgbX
from .zones import GERMANY, Zone

KEEP_DAYS = 740  # the 730-day window plus room for the lags
MODELS = ["bmtx", "learx", "xgbx"]
BIAS_DAYS = 28
COLUMNS = ["forecast_made_utc", "date", "hour", *MODELS, "naive_day", "naive_mix", "ensemble", "ensemble_bc"]


class LiveFiles:
    """The rolling inputs in <folder>/live, one CSV each with a UTC `time` column."""

    def __init__(self, folder: Path, zone: Zone):
        self.zone, self.dir = zone, Path(folder) / "live"
        self.files = {"prices": f"prices_{zone.code.lower()}_live.csv", "neighbours": "neighbours_live.csv",
                      "weather": "weather_live.csv", "weather_germany": "weather_de_live.csv"}

    def path(self, name: str) -> Path:
        return self.dir / self.files[name]

    def read(self, name: str) -> pd.DataFrame:
        frame = pd.read_csv(self.path(name))
        return frame.set_index(pd.DatetimeIndex(pd.to_datetime(frame.pop("time"), utc=True), name="time"))

    def prices(self) -> pd.Series:
        return self.read("prices")["price"]

    def neighbours(self) -> pd.DataFrame:
        return self.read("neighbours").rename(columns=lambda c: c.removeprefix("price_"))

    def weather(self) -> pd.DataFrame:
        return self.read("weather")

    def weather_germany(self) -> pd.DataFrame:
        return self.read("weather_germany")

    # updating

    def _merge(self, name: str, new: pd.DataFrame, cutoff: pd.Timestamp | None):
        """New rows replace old ones for the same hour, and rows older than the cutoff go."""
        old = self.read(name) if self.path(name).exists() else None
        frame = new if old is None else pd.concat([new, old])[[*old.columns, *new.columns.difference(old.columns)]]
        frame = frame[~frame.index.duplicated()].sort_index()
        if cutoff is not None:
            frame = frame[frame.index >= cutoff]
        self.dir.mkdir(parents=True, exist_ok=True)
        out = frame.rename_axis("time").reset_index()
        out["time"] = out["time"].dt.strftime("%Y-%m-%dT%H:%M:%SZ")
        out.to_csv(self.path(name), index=False, na_rep="NA")
        print(f"{name}: {frame.index.min():%Y-%m-%d %H:%M} to {frame.index.max():%Y-%m-%d %H:%M} UTC, {len(frame)} hours", flush=True)

    def _since(self, name: str, back: int, first: date) -> date:
        """Refetch the last `back` days, which may have been incomplete, or everything from `first` on a first run."""
        if not self.path(name).exists():
            return first
        return self.read(name).index.max().date() - timedelta(days=back)

    def update(self, today: date | None = None):
        today = today or datetime.now(timezone.utc).date()
        first = today - timedelta(days=KEEP_DAYS)
        cutoff = pd.Timestamp(first, tz="UTC")
        end = f"{today + timedelta(days=2)}"
        # the zone's own prices have to update, the rest may fail and leave a gap the forecast works around
        since = self._since("prices", 1, first)
        self._merge("prices", area_prices([self.zone.code], f"{since}", end)[self.zone.code].dropna().to_frame("price"), cutoff)
        steps = {
            "neighbours": lambda: area_prices(list(self.zone.neighbours), f"{self._since('neighbours', 1, first)}", end).add_prefix("price_"),
            "weather": lambda: weather(self.zone.sites, f"{self._since('weather', 2, first)}", f"{today + timedelta(days=1)}"),
            "weather_germany": lambda: weather(GERMANY, f"{self._since('weather_germany', 2, date(2024, 3, 1))}",
                                               f"{today + timedelta(days=1)}"),
        }
        for name, fetch in steps.items():
            try:
                new = fetch()
                if name != "neighbours":
                    new = new.dropna()  # an hour with any site missing is fetched again next time
                self._merge(name, new, None if name == "weather_germany" else cutoff)
            except Exception as err:  # noqa: BLE001 - one source failing must not stop the forecast
                print(f"{name} not updated: {err}", flush=True)


def xgb_settings(folder: Path) -> XgbX:
    """The depth and number of rounds chosen when the model was tuned."""
    tuning = pd.read_csv(Path(folder) / "results" / "xgb_tuning.csv")
    chosen = tuning[(tuning["model"] == "xgbx") & tuning["chosen"]].iloc[0]
    return XgbX(max_depth=int(chosen["max_depth"]), rounds=int(chosen["rounds"]))


def read_log(path: Path) -> pd.DataFrame:
    return pd.read_csv(path, dtype={"forecast_made_utc": str}, parse_dates=["date"])


def ensemble_bias(folder: Path, features: Features, target: pd.Timestamp) -> float:
    """The median error of the ensemble over the last 28 days, from the backtest and the live log. Not shown on the
    dashboard yet: a challenger, saved so it can be judged on days it never saw."""
    backtest = pd.read_csv(Path(folder) / "results" / "backtest_forecasts.csv", parse_dates=["date"])
    live = read_log(Path(folder) / "forecasts" / "forecasts_live.csv")
    for frame in (backtest, live):
        for m in MODELS:
            if m not in frame:
                frame[m] = np.nan
        frame["ensemble"] = frame[MODELS].mean(axis=1, skipna=False)
    actual = pd.DataFrame({"date": np.repeat(features.dates, 24), "hour": np.tile(np.arange(24), len(features.dates)),
                           "actual": features.P.ravel()})
    live = live[live["date"] > backtest["date"].max()].merge(actual, on=["date", "hour"])
    live = live.dropna(subset=["actual", "ensemble"]).groupby("date").filter(lambda g: len(g) == 24)
    past = pd.concat([backtest, live])[["date", "actual", "ensemble"]]
    past = past[(past["date"] >= target - pd.Timedelta(days=BIAS_DAYS)) & (past["date"] < target)]
    if past["date"].nunique() < 0.8 * BIAS_DAYS:
        raise ValueError("too few days for the bias")
    return float(np.median(past["actual"] - past["ensemble"]))  # a gap anywhere gives no bias, as before


class DailyForecast:
    """Tomorrow's prices for one zone from the files in its folder."""

    def __init__(self, folder: Path, zone: Zone):
        self.folder, self.zone, self.files = Path(folder), zone, LiveFiles(folder, zone)
        self.models = [BmtX(), LearX(), xgb_settings(folder)]

    def features(self, target: pd.Timestamp) -> Features:
        """Only prices from before the delivery day, so a run after the auction can't see the answer."""
        before = lambda frame: frame[frame.index.tz_convert(TZ).tz_localize(None).normalize() < target]
        return Features(before(self.files.prices()), before(self.files.neighbours()), self.files.weather(),
                        self.files.weather_germany(), extra_day=target)

    def build(self, target: pd.Timestamp, made: datetime | None = None) -> pd.DataFrame:
        features = self.features(target)
        day = int(np.flatnonzero(features.dates == target)[0])
        groups = features.available_groups(day)
        if groups != GROUPS:
            print(f"Weather missing for {target:%Y-%m-%d}, models run with: {', '.join(groups)}", flush=True)
        out = {}
        for model in self.models:
            try:
                out[model.name] = np.round(model.forecast(features, day, groups), 2)
            except Exception as err:  # noqa: BLE001 - a failed model leaves a gap, the others are still saved
                print(f"{model.name} failed: {err}", flush=True)
                out[model.name] = np.full(24, np.nan)
        naive_day = features.P[day - 1]
        naive_mix = features.P[day - 7] if features.dow[day] in (1, 6, 7) else naive_day  # last week for mon, sat, sun
        try:
            bias = ensemble_bias(self.folder, features, target)
        except Exception as err:  # noqa: BLE001
            print(f"bias failed: {err}", flush=True)
            bias = np.nan
        frame = pd.DataFrame({"forecast_made_utc": f"{made or datetime.now(timezone.utc):%Y-%m-%d %H:%M}",
                              "date": f"{target:%Y-%m-%d}", "hour": range(24), **out,
                              "naive_day": np.round(naive_day, 2), "naive_mix": np.round(naive_mix, 2)})
        ensemble = frame[MODELS].mean(axis=1, skipna=False)
        frame["ensemble"], frame["ensemble_bc"] = ensemble.round(2), (ensemble + bias).round(2)
        return frame[COLUMNS]


def number(x) -> str:
    """As R's write_csv writes them: NA for a gap and no trailing zeros."""
    if isinstance(x, str):
        return x
    if x is None or (isinstance(x, float) and math.isnan(x)):
        return "NA"
    return f"{x:.2f}".rstrip("0").rstrip(".") if isinstance(x, float) else str(x)


def append(log: Path, new: pd.DataFrame) -> str:
    """Adds the rows as text at the end, so the existing track record is never rewritten. A day already there is kept."""
    if not log.exists():
        if os.environ.get("GITHUB_ACTIONS") == "true":
            raise FileNotFoundError(f"{log} is missing - restore it from the git history instead of starting a new track record")
        log.parent.mkdir(parents=True, exist_ok=True)
        log.write_text(",".join(new.columns) + "\n")
    text = log.read_text()
    header = text.splitlines()[0].split(",")
    target = new["date"].iloc[0]
    if any(line.split(",")[header.index("date")] == target for line in text.splitlines()[1:]):
        return f"Forecast for {target} already exists - not overwriting."
    rows = new.reindex(columns=header)  # an older log may have retired columns, left empty
    lines = [",".join(number(v) for v in row) for row in rows.itertuples(index=False)]
    with log.open("a", newline="\n") as f:
        f.write(("" if text.endswith("\n") else "\n") + "\n".join(lines) + "\n")
    return f"Forecast for {target} added."


def run(folder: Path, zone: Zone, log: Path, now: datetime | None = None, preview: bool = False) -> str:
    now = now or datetime.now(timezone.utc)
    local = pd.Timestamp(now).tz_convert(TZ)
    target = pd.Timestamp(local.date() + timedelta(days=1))
    frame = DailyForecast(folder, zone).build(target, now)
    print(frame.to_string(index=False), flush=True)
    if local.hour >= 12 and not preview:
        return f"After the noon auction ({local:%H:%M} danish time) - no forecast saved for {target:%Y-%m-%d}"
    return append(log, frame)
