"""The three price models behind one interface: each gives tomorrow's 24 prices from everything known before the auction."""
from abc import ABC, abstractmethod

import numpy as np
import xgboost as xgb
from scipy.stats import norm
from sklearn.linear_model import lasso_path

from .features import Features

WINDOW = 730


class PriceModel(ABC):
    name: str

    @abstractmethod
    def forecast(self, features: Features, day: int) -> np.ndarray:
        """24 prices in EUR/MWh."""


def select_by_testing(E: np.ndarray, y: np.ndarray, df: int, ss: np.ndarray, pval: float = 0.05) -> list[int]:
    """Boosting with multiple testing: add the most significant input while it passes a Bonferroni-style bar. E and y are
    already residuals on the controls. Each pick is partialled out of the rest, which avoids refitting a regression per round."""
    E, y = E.copy(), y.copy()
    bar = norm.ppf(1 - pval / (2 * E.shape[1]))
    selected: list[int] = []
    while True:
        sxx = (E ** 2).sum(axis=0)
        sxy = E.T @ y
        with np.errstate(divide="ignore", invalid="ignore"):
            ssr = (y ** 2).sum() - sxy ** 2 / sxx
            t = np.abs(sxy) / np.sqrt(sxx * ssr / (df - len(selected) - 1))
        t[selected] = np.nan
        t[(sxx < 1e-12 * ss) | (sxx == 0)] = np.nan
        if np.isnan(t).all():
            return selected
        best = int(np.nanargmax(t))
        if t[best] <= bar:
            return selected
        selected.append(best)
        e = E[:, best].copy()
        ee = e @ e
        E -= np.outer(e, e @ E) / ee
        y -= e * (e @ y) / ee


class BmtX(PriceModel):
    """One linear model per hour, on the inputs that survive strict significance testing."""
    name = "bmtx"

    def forecast(self, features: Features, day: int) -> np.ndarray:
        inputs = features.linear_inputs(day, WINDOW)
        keep = inputs.keep
        X = inputs.X[keep]
        controls = np.column_stack([np.ones(len(keep)), features.X_dow[keep, 1:]])  # monday is the baseline
        Q, _ = np.linalg.qr(controls)
        residual = lambda M: M - Q @ (Q.T @ M)
        E0, ss = residual(X), (X ** 2).sum(axis=0)
        df = len(keep) - controls.shape[1]
        today = np.r_[1.0, features.X_dow[day, 1:]]
        out = np.empty(24)
        for hour in range(24):
            y = inputs.target(features.P[keep, hour])
            chosen = select_by_testing(E0, residual(y), df, ss)
            design = np.column_stack([controls, X[:, chosen]])
            beta = np.linalg.lstsq(design, y, rcond=None)[0]
            out[hour] = inputs.invert(np.r_[today, inputs.X[day, chosen]] @ beta)
        return out


def lasso_bic(X: np.ndarray, y: np.ndarray, n_alphas: int = 100, min_ratio: float = 1e-4) -> tuple[float, np.ndarray]:
    """Lasso on standardised inputs, with the penalty chosen by BIC along a path. Same objective and path as glmnet.
    Returns the intercept and coefficients on the original scale."""
    n = len(y)
    x_mean, x_sd = X.mean(axis=0), X.std(axis=0)
    used = x_sd > 0
    Z = (X[:, used] - x_mean[used]) / x_sd[used]
    y_mean = y.mean()
    yc = y - y_mean
    top = np.abs(Z.T @ yc).max() / n
    alphas = top * np.geomspace(1, min_ratio, n_alphas)
    _, paths, _ = lasso_path(Z, yc, alphas=alphas, tol=1e-7, max_iter=20_000)
    rss = ((yc[:, None] - Z @ paths) ** 2).sum(axis=0)
    bic = n * np.log(rss / n) + (paths != 0).sum(axis=0) * np.log(n)
    best = paths[:, int(np.argmin(bic))] / x_sd[used]
    coef = np.zeros(X.shape[1])
    coef[used] = best
    return float(y_mean - x_mean @ coef), coef


class LearX(PriceModel):
    """One lasso per hour, all inputs at once, with the useless ones shrunk to zero."""
    name = "learx"

    def forecast(self, features: Features, day: int) -> np.ndarray:
        inputs = features.linear_inputs(day, WINDOW)
        keep = inputs.keep
        X = np.column_stack([inputs.X[keep], features.X_dow[keep]])
        today = np.r_[inputs.X[day], features.X_dow[day]]
        out = np.empty(24)
        for hour in range(24):
            intercept, coef = lasso_bic(X, inputs.target(features.P[keep, hour]))
            out[hour] = inputs.invert(intercept + today @ coef)
        return out


class XgbX(PriceModel):
    """One boosted-tree model for all hours, seeded by the date so a day can be reproduced."""
    name = "xgbx"

    def __init__(self, max_depth: int = 4, rounds: int = 750):
        self.params = dict(objective="reg:squarederror", learning_rate=0.05, max_depth=max_depth, min_child_weight=10,
                           subsample=0.8, colsample_bytree=0.8, nthread=1)
        self.rounds = rounds

    def forecast(self, features: Features, day: int) -> np.ndarray:
        train = np.arange(day - WINDOW, day)
        median, scale = features.scales(train)
        X_today = features.tree_design(np.array([day]))
        if np.isnan(X_today).any():
            raise ValueError(f"missing inputs for {features.dates[day]:%Y-%m-%d}")
        X = features.tree_design(train)
        y = np.arcsinh((features.P[train].ravel() - median) / scale)
        keep = ~np.isnan(y)
        seed = int((features.dates[day] - np.datetime64("1970-01-01")) / np.timedelta64(1, "D"))
        booster = xgb.train({**self.params, "seed": seed}, xgb.DMatrix(X[keep], label=y[keep]), num_boost_round=self.rounds)
        return median + scale * np.sinh(booster.predict(xgb.DMatrix(X_today)))


def all_models() -> list[PriceModel]:
    return [BmtX(), LearX(), XgbX()]
