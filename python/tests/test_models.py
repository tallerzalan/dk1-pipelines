"""Offline tests: each uses a case with a known answer, computed without the code under test."""
import numpy as np
import pandas as pd

from dkfc.backtest import compare, dm_test
from dkfc.features import Features, daily_matrix, danish_holidays, easter, lag, mad
from dkfc.models import lasso_bic, select_by_testing
from dkfc.zones import ZONES

rng = np.random.default_rng(7)


def test_mad_matches_r_definition():
    assert np.isclose(mad(np.array([1.0, 2.0, 3.0, 4.0, 100.0])), 1.4826)


def test_easter_known_years():
    assert easter(2024) == pd.Timestamp("2024-03-31") and easter(2026) == pd.Timestamp("2026-04-05")


def test_store_bededag_ended_in_2024():
    assert pd.Timestamp("2023-05-05") in danish_holidays([2023])
    assert pd.Timestamp("2024-04-26") not in danish_holidays([2024])
    assert pd.Timestamp("2026-06-05") in danish_holidays([2026])


def test_daily_matrix_handles_both_clock_changes():
    t = pd.date_range("2026-03-28 23:00", "2026-03-29 21:00", freq="h", tz="UTC")  # the 23-hour day
    m = daily_matrix(pd.Series(np.arange(len(t), dtype=float), index=t), pd.DatetimeIndex(["2026-03-29"]))
    assert not np.isnan(m).any() and m[0, 2] == (m[0, 1] + m[0, 3]) / 2
    t = pd.date_range("2026-10-24 22:00", "2026-10-25 22:00", freq="h", tz="UTC")  # the 25-hour day
    m = daily_matrix(pd.Series(np.arange(len(t), dtype=float), index=t), pd.DatetimeIndex(["2026-10-25"]))
    assert m[0, 2] == 2.5  # the double 02:00 is averaged


def test_lag_uses_only_the_past():
    m = np.arange(12.0).reshape(3, 4)
    assert np.isnan(lag(m, 1)[0]).all() and (lag(m, 1)[1] == m[0]).all()


def test_selection_finds_the_real_input_and_ignores_noise():
    X = rng.normal(size=(500, 50))
    y = 3 * X[:, 7] - 2 * X[:, 21] + rng.normal(size=500)
    Xc, yc = X - X.mean(axis=0), y - y.mean()
    assert set(select_by_testing(Xc, yc, 498, (X ** 2).sum(axis=0))) == {7, 21}


def test_selection_picks_nothing_from_pure_noise():
    X = rng.normal(size=(500, 50))
    assert select_by_testing(X - X.mean(axis=0), rng.normal(size=500), 498, (X ** 2).sum(axis=0)) == []


def test_lasso_recovers_a_sparse_truth_and_handles_a_constant_column():
    X = np.column_stack([rng.normal(size=(600, 30)), np.ones(600)])
    y = 5.0 + 2 * X[:, 3] - X[:, 9] + rng.normal(scale=0.2, size=600)
    intercept, coef = lasso_bic(X, y)
    assert set(np.flatnonzero(np.abs(coef) > 0.1)) == {3, 9}
    assert abs(coef[3] - 2) < 0.1 and abs(intercept - 5) < 0.1 and coef[30] == 0


def test_zones_share_a_shape():
    for zone in ZONES.values():
        assert len(zone.neighbours) == 5 and zone.code not in zone.neighbours
        assert {s.kind for s in zone.sites} == {"wind", "solar"}


def synthetic(days=800):
    t = pd.date_range("2024-01-01", periods=24 * days, freq="h", tz="UTC")
    wave = 60 + 20 * np.sin(2 * np.pi * t.hour / 24)
    prices = pd.Series(wave + rng.normal(0, 5, len(t)), index=t)
    nb = pd.DataFrame({a: wave + rng.normal(0, 5, len(t)) for a in ZONES["DK1"].neighbours}, index=t)
    wx = pd.DataFrame({"wind_a": rng.uniform(5, 40, len(t)), "solar_a": rng.uniform(0, 500, len(t))}, index=t)
    return Features(prices, nb, wx, wx)


def test_inputs_do_not_change_with_future_prices():
    f = synthetic()
    day = 760
    before = f.linear_inputs(day).X[day].copy()
    f.P[day:] += 1000
    f.lags = {k: lag(f.P, k) for k in f.lags}
    f.X_price = np.hstack([f.lags[k] for k in f.lags])
    assert np.allclose(before, f.linear_inputs(day).X[day])


def test_design_matrices_have_the_expected_size():
    f = synthetic()
    assert f.linear_inputs(760).X.shape[1] == 96 + 5 * 24 + 2 + 4 * 24
    assert f.tree_design(np.array([760])).shape == (24, 2 + 4 + 4 + 2 * 9 + 2)


def test_compare_passes_equal_forecasts_and_fails_clearly_worse_ones():
    dates = pd.date_range("2026-01-01", periods=60).repeat(24)
    base = pd.DataFrame({"date": dates, "hour": np.tile(range(24), 60), "actual": rng.normal(50, 10, 1440)})
    for m in ("bmtx", "learx", "xgbx"):
        base[m] = base["actual"] + rng.normal(0, 5, 1440)
    assert compare(base[["date", "hour", "bmtx", "learx", "xgbx"]], base)["passes"].all()
    worse = base[["date", "hour"]].assign(**{m: base[m] + 20 for m in ("bmtx", "learx", "xgbx")})
    assert not compare(worse, base)["passes"].any()


def test_dm_test_sees_a_consistent_difference():
    assert dm_test(rng.normal(1, 0.5, 200)) < 0.001 and dm_test(rng.normal(0, 1, 200)) > 0.001
