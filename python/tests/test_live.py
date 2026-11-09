import numpy as np
import pandas as pd
import pytest

from dkfc.features import GROUPS, Features
from dkfc.live import append, number


def test_numbers_are_written_as_r_wrote_them():
    assert [number(x) for x in (114.0, 113.9, 113.94, -0.5, np.nan, np.float64(2.0), 7, "2026-11-10")] == \
           ["114", "113.9", "113.94", "-0.5", "NA", "2", "7", "2026-11-10"]


def frame(day: str, value: float) -> pd.DataFrame:
    return pd.DataFrame({"forecast_made_utc": "2026-11-09 09:19", "date": day, "hour": range(24), "bmtx": value,
                         "ensemble": np.nan})


def test_append_keeps_the_track_record(tmp_path):
    log = tmp_path / "forecasts_live.csv"
    log.write_text("forecast_made_utc,date,hour,lear,bmtx,ensemble\n2026-11-08 09:19,2026-11-09,0,NA,1.5,NA\n")
    before = log.read_text()
    assert append(log, frame("2026-11-10", 2.0)).endswith("added.")
    text = log.read_text()
    assert text.startswith(before)  # nothing already there is rewritten
    assert text.splitlines()[-1] == "2026-11-09 09:19,2026-11-10,23,NA,2,NA"  # a retired column stays empty
    assert append(log, frame("2026-11-10", 3.0)).endswith("not overwriting.")
    assert log.read_text() == text


def test_append_refuses_to_start_a_new_log_on_github(tmp_path, monkeypatch):
    monkeypatch.setenv("GITHUB_ACTIONS", "true")
    with pytest.raises(FileNotFoundError):
        append(tmp_path / "forecasts_live.csv", frame("2026-11-10", 1.0))


def test_a_weather_gap_on_the_day_drops_only_that_group():
    hours = pd.date_range("2026-01-01", "2026-01-10 23:00", freq="h", tz="UTC")
    prices = pd.Series(np.arange(len(hours), dtype=float), index=hours)
    neighbours = pd.DataFrame({"DE": prices.to_numpy()}, index=hours)
    weather = pd.DataFrame({"wind_a": 30.0, "solar_a": 100.0}, index=hours)
    germany = weather.rename(columns=lambda c: c.replace("_a", "_de"))
    day = lambda f: int(np.flatnonzero(f.dates == "2026-01-09")[0])  # a whole danish day inside the data
    full = Features(prices, neighbours, weather, germany)
    assert full.available_groups(day(full)) == GROUPS
    gap = Features(prices, neighbours, weather[weather.index < "2026-01-09 12:00"], germany)
    assert gap.available_groups(day(gap)) == ("nb", "hol", "wxde")
