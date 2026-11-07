"""python -m dkfc parity --zone DK1 --reference ../price/results/backtest_forecasts.csv --out results/parity_DK1"""
import argparse
from pathlib import Path

import pandas as pd

from .backtest import compare, run_backtest
from .data import MarketData
from .features import Features
from .zones import ZONES


def main() -> int:
    parser = argparse.ArgumentParser(prog="dkfc")
    parser.add_argument("command", choices=["parity"])
    parser.add_argument("--zone", choices=list(ZONES), required=True)
    parser.add_argument("--reference", type=Path, required=True, help="the R backtest to compare with")
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--cache", type=Path, default=Path("data"))
    args = parser.parse_args()

    market = MarketData(ZONES[args.zone], args.cache)
    features = Features(market.prices(), market.neighbours(), market.weather(), market.weather_germany())
    reference = pd.read_csv(args.reference, parse_dates=["date"])
    ours = run_backtest(features, str(reference["date"].min().date()), str(reference["date"].max().date()))
    args.out.mkdir(parents=True, exist_ok=True)
    ours.round(2).to_csv(args.out / "forecasts.csv", index=False)
    result = compare(ours, reference)
    result.round(4).to_csv(args.out / "comparison.csv", index=False)
    print(result.round(4).to_string(index=False))
    return 0 if result["passes"].all() else 1


if __name__ == "__main__":
    raise SystemExit(main())
