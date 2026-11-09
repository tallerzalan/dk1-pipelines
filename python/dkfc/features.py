"""Turns raw market data into the day-by-24-hour matrices the models use, with every scaling fitted on the training window only."""
from dataclasses import dataclass

import numpy as np
import pandas as pd

TZ = "Europe/Copenhagen"
PRICE_LAGS = (1, 2, 3, 7)
MAD_SCALE = 1.4826
# the input groups beside the zone's own prices (see inputs.csv). a weather group missing on the forecast day is left out
GROUPS = ("nb", "hol", "wx", "wxde")
WEATHER_GROUPS = {"wx": ("wind", "solar"), "wxde": ("de_wind", "de_solar")}


def mad(x: np.ndarray) -> float:
    return MAD_SCALE * np.nanmedian(np.abs(x - np.nanmedian(x)))


def daily_matrix(values: pd.Series, dates: pd.DatetimeIndex) -> np.ndarray:
    """Days x 24 in Danish time. The autumn's double hour is averaged and the spring's missing 02:00 is interpolated."""
    local = values.index.tz_convert(TZ)
    frame = pd.DataFrame({"date": local.tz_localize(None).normalize(), "hour": local.hour, "value": values.to_numpy()})
    wide = frame.groupby(["date", "hour"])["value"].mean().unstack("hour").reindex(index=dates, columns=range(24))
    out = wide.to_numpy(dtype=float, copy=True)
    gap = np.isnan(out[:, 2])
    out[gap, 2] = (out[gap, 1] + out[gap, 3]) / 2
    return out


def lag(matrix: np.ndarray, days: int) -> np.ndarray:
    return np.vstack([np.full((days, matrix.shape[1]), np.nan), matrix[:-days]])


def easter(year: int) -> pd.Timestamp:
    a, b, c = year % 19, year // 100, year % 100
    d, e, f = b // 4, b % 4, (b + 8) // 25
    h = (19 * a + b - d - (b - f + 1) // 3 + 15) % 30
    i, k = c // 4, c % 4
    l = (32 + 2 * e + 2 * i - h - k) % 7
    m = (a + 11 * h + 22 * l) // 451
    n = h + l - 7 * m + 114
    return pd.Timestamp(year, n // 31, n % 31 + 1)


def danish_holidays(years) -> set[pd.Timestamp]:
    days = set()
    for y in years:
        e = easter(y)
        moving = [-3, -2, 0, 1, 39, 49, 50] + ([26] if y < 2024 else [])  # store bededag until 2023
        days |= {e + pd.Timedelta(days=k) for k in moving}
        days |= {pd.Timestamp(y, m, d) for m, d in ((1, 1), (6, 5), (12, 24), (12, 25), (12, 26), (12, 31))}
    return days


def wind_power(weather: pd.DataFrame) -> pd.Series:
    """Average wind through a simple power curve. Speeds arrive in km/h."""
    speed = weather.filter(like="wind_").mean(axis=1, skipna=False) / 3.6
    return ((speed - 3) / 9).clip(0, 1) ** 3


def radiation(weather: pd.DataFrame) -> pd.Series:
    """Radiation is the mean of the hour before its timestamp, so it moves back an hour."""
    sun = weather.filter(like="solar_").mean(axis=1, skipna=False)
    return pd.Series(sun.to_numpy(), index=sun.index - pd.Timedelta(hours=1))


@dataclass
class Inputs:
    """One forecast day's design matrix, on the transformed scale."""
    X: np.ndarray
    keep: np.ndarray
    median: float
    scale: float

    def target(self, prices: np.ndarray) -> np.ndarray:
        return np.arcsinh((prices - self.median) / self.scale)

    def invert(self, z):
        return self.median + self.scale * np.sinh(z)


class Features:
    """Everything known about each delivery day, as matrices aligned on `dates`."""

    def __init__(self, prices: pd.Series, neighbours: pd.DataFrame, weather: pd.DataFrame, weather_germany: pd.DataFrame,
                 extra_day: pd.Timestamp | None = None):
        local = prices.index.tz_convert(TZ).tz_localize(None).normalize()
        last = max(local.max(), extra_day) if extra_day is not None else local.max()
        self.dates = pd.date_range(local.min(), last, freq="D")
        self.P = daily_matrix(prices, self.dates)
        self.dow = self.dates.dayofweek.to_numpy() + 1  # 1 = monday
        self.X_dow = np.eye(7)[self.dow - 1]
        self.lags = {k: lag(self.P, k) for k in PRICE_LAGS}
        self.X_price = np.hstack([self.lags[k] for k in PRICE_LAGS])
        holidays = danish_holidays(range(self.dates[0].year, self.dates[-1].year + 2))
        self.hol = np.column_stack([self.dates.isin(holidays), (self.dates - pd.Timedelta(days=1)).isin(holidays)]).astype(float)
        self.neighbours = {a.lower(): lag(daily_matrix(neighbours[a].dropna(), self.dates), 1) for a in neighbours.columns}
        self.weather = {"wind": daily_matrix(wind_power(weather), self.dates), "solar": daily_matrix(radiation(weather), self.dates),
                        "de_wind": daily_matrix(wind_power(weather_germany), self.dates),
                        "de_solar": daily_matrix(radiation(weather_germany), self.dates)}

    def _weather(self, groups) -> list[np.ndarray]:
        return [self.weather[k] for g in WEATHER_GROUPS if g in groups for k in WEATHER_GROUPS[g]]

    def available_groups(self, day: int) -> tuple[str, ...]:
        """The groups a forecast for `day` can use: a weather group with a gap on that day is dropped, so there is
        still a forecast."""
        return tuple(g for g in GROUPS if g not in WEATHER_GROUPS
                     or not any(np.isnan(self.weather[k][day]).any() for k in WEATHER_GROUPS[g]))

    def complete_days(self, start: str) -> np.ndarray:
        return np.flatnonzero((self.dates >= start) & ~np.isnan(self.P).any(axis=1))

    def scales(self, train: np.ndarray) -> tuple[float, float]:
        window = self.P[train]
        return float(np.nanmedian(window)), mad(window)

    def linear_inputs(self, day: int, window: int = 730, groups=GROUPS) -> Inputs:
        """Design matrix for the linear models. Refuses a day with missing inputs, so nothing is silently imputed."""
        train = np.arange(day - window, day)
        median, scale = self.scales(train)
        blocks = [np.arcsinh((self.X_price - median) / scale)]
        for matrix in self.neighbours.values() if "nb" in groups else ():
            tr = matrix[train]
            s = mad(tr)
            s = s if np.isfinite(s) and s > 0 else np.nanstd(tr, ddof=1)
            blocks.append(np.arcsinh((matrix - np.nanmedian(tr)) / s))
        if "hol" in groups:
            blocks.append(self.hol)
        for matrix in self._weather(groups):
            tr = matrix[train]
            blocks.append((matrix - np.nanmean(tr)) / np.nanstd(tr, ddof=1))
        X = np.hstack(blocks)
        if np.isnan(X[day]).any():
            raise ValueError(f"missing inputs for {self.dates[day]:%Y-%m-%d}")
        complete = ~np.isnan(X[train]).any(axis=1) & ~np.isnan(self.P[train]).any(axis=1)
        return Inputs(X, train[complete], median, scale)

    def tree_design(self, rows: np.ndarray, groups=GROUPS) -> np.ndarray:
        """One row per day and hour, for the tree model. Trees handle missing values, so training rows may have gaps."""
        rep = lambda x: np.repeat(x, 24)
        d1 = self.lags[1][rows]
        columns = [np.tile(np.arange(24), len(rows)), rep(self.dow[rows])]
        columns += [self.lags[k][rows].ravel() for k in PRICE_LAGS]
        columns += [rep(d1.mean(axis=1)), rep(d1.min(axis=1)), rep(d1.max(axis=1)), rep(d1[:, 23])]
        neighbours = list(self.neighbours.values()) if "nb" in groups else []
        for matrix in [*neighbours, *self._weather(groups)]:
            m = matrix[rows]
            columns += [m.ravel(), rep(m.mean(axis=1))]
        if "hol" in groups:
            columns += [rep(self.hol[rows, 0]), rep(self.hol[rows, 1])]
        return np.column_stack(columns)
