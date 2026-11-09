"""Command line:
  python -m dkfc update   --zone DK1 --folder ../price       bring the rolling input files up to date
  python -m dkfc forecast --zone DK1 --folder ../price       add tomorrow's prices to the log, before the noon auction
  python -m dkfc parity   --zone DK1 --reference ../price/results/backtest_forecasts.csv --out results/parity_DK1
"""
import argparse
import time
from pathlib import Path

import pandas as pd

from . import live
from .backtest import compare, run_backtest
from .data import MarketData
from .features import Features
from .zones import ZONES


def parity(args) -> int:
    began = time.perf_counter()
    stage = lambda name: print(f"[{(time.perf_counter() - began) / 60:5.1f} min] {name}", flush=True)
    market = MarketData(ZONES[args.zone], args.cache)
    stage("prices")
    prices = market.prices()
    stage("neighbour prices")
    neighbours = market.neighbours()
    stage("weather")
    local_weather = market.weather()
    stage("german weather")
    german_weather = market.weather_germany()
    stage("features")
    features = Features(prices, neighbours, local_weather, german_weather)
    stage("backtest")
    reference = pd.read_csv(args.reference, parse_dates=["date"])
    ours = run_backtest(features, str(reference["date"].min().date()), str(reference["date"].max().date()))
    args.out.mkdir(parents=True, exist_ok=True)
    ours.round(2).to_csv(args.out / "forecasts.csv", index=False)
    result = compare(ours, reference)
    result.round(4).to_csv(args.out / "comparison.csv", index=False)
    print(result.round(4).to_string(index=False))
    return 0 if result["passes"].all() else 1


def main() -> int:
    parser = argparse.ArgumentParser(prog="dkfc")
    commands = parser.add_subparsers(dest="command", required=True)
    for name in ("update", "forecast", "parity"):
        sub = commands.add_parser(name)
        sub.add_argument("--zone", choices=list(ZONES), required=True)
        if name == "parity":
            sub.add_argument("--reference", type=Path, required=True, help="the R backtest to compare with")
            sub.add_argument("--out", type=Path, required=True)
            sub.add_argument("--cache", type=Path, default=Path("data"))
        else:
            sub.add_argument("--folder", type=Path, required=True, help="the zone's folder, with live/, results/ and forecasts/")
        if name == "forecast":
            sub.add_argument("--log", type=Path, help="default: forecasts/forecasts_live.csv in the folder")
            sub.add_argument("--preview", action="store_true", help="save even after the noon auction")
    args = parser.parse_args()
    zone = ZONES[args.zone]
    if args.command == "update":
        live.LiveFiles(args.folder, zone).update()
    elif args.command == "forecast":
        print(live.run(args.folder, zone, args.log or args.folder / "forecasts" / "forecasts_live.csv", preview=args.preview))
    else:
        return parity(args)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
