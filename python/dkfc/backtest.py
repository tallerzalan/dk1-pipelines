"""Rolling backtest, and the comparison that decides whether Python may replace R."""
import time

import numpy as np
import pandas as pd
from joblib import Parallel, delayed
from scipy.stats import t as student_t

from .features import Features
from .models import PriceModel, all_models

MODELS = ["bmtx", "learx", "xgbx"]


def one_day(features: Features, day: int, models: list[PriceModel]) -> pd.DataFrame:
    """A model that can't forecast a day leaves a gap, so one failure doesn't stop the run."""
    frames = []
    for model in models:
        try:
            forecast = model.forecast(features, day)
        except (ValueError, np.linalg.LinAlgError):
            continue
        frames.append(pd.DataFrame({"date": features.dates[day], "hour": range(24), "model": model.name, "forecast": forecast}))
    return pd.concat(frames) if frames else pd.DataFrame()


def run_backtest(features: Features, start: str = "2025-10-01", end: str | None = None, jobs: int = -1) -> pd.DataFrame:
    days = features.complete_days(start)
    if end:
        days = days[features.dates[days] <= pd.Timestamp(end)]
    models = all_models()
    began = time.perf_counter()
    frames = Parallel(n_jobs=jobs, verbose=5)(delayed(one_day)(features, int(d), models) for d in days)
    print(f"{len(days)} days in {(time.perf_counter() - began) / 60:.1f} minutes")
    wide = pd.concat(frames).pivot_table(index=["date", "hour"], columns="model", values="forecast").reset_index()
    wide.columns.name = None
    return wide


def dm_test(d: np.ndarray, lag: int = 7) -> float:
    """Diebold-Mariano p-value on daily loss differences, with a Newey-West variance and the HLN small-sample factor."""
    n, u = len(d), d - d.mean()
    v = (u @ u) / n
    for k in range(1, min(lag, n - 1) + 1):
        v += 2 * (1 - k / (lag + 1)) * (u[k:] @ u[:-k]) / n
    if v <= 0:  # identical forecasts
        return 1.0
    stat = d.mean() / np.sqrt(v / n) * np.sqrt((n - 1) / n)
    return float(2 * student_t.sf(abs(stat), n - 1))


def compare(ours: pd.DataFrame, reference: pd.DataFrame, max_worse: float = 0.02) -> pd.DataFrame:
    """Python passes a model if, on the same hours, its error is at most `max_worse` above R's, or isn't significantly worse."""
    joined = ours.merge(reference, on=["date", "hour"], suffixes=("", "_r"))
    for frame_suffix in ("", "_r"):
        joined["ensemble" + frame_suffix] = joined[[m + frame_suffix for m in MODELS]].mean(axis=1, skipna=False)
    joined = joined.dropna(subset=["actual", "ensemble", "ensemble_r"])
    rows = []
    for m in [*MODELS, "ensemble"]:
        py, r = (joined[m] - joined["actual"]).abs(), (joined[m + "_r"] - joined["actual"]).abs()
        daily = (py - r).groupby(joined["date"]).mean().to_numpy()
        p = dm_test(daily)
        change = py.mean() / r.mean() - 1
        rows.append({"model": m, "hours": len(joined), "mae_python": py.mean(), "mae_r": r.mean(), "change": change, "dm_p": p,
                     "mean_abs_gap": (joined[m] - joined[m + "_r"]).abs().mean(), "corr": joined[m].corr(joined[m + "_r"]),
                     "passes": bool(change <= max_worse or p >= 0.05)})
    return pd.DataFrame(rows)
