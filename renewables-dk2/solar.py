# round 2 for solar, by the rules in the readme: each change is tried on march-december 2025 and kept if it
# lowers the error there. the verdict against energinet comes from live days, not from 2026

import json
from pathlib import Path

import pandas as pd

import improve as im

cfg = json.loads(Path("results/improve_choice.json").read_text())["solar"]
best = im.mae(im.backtest("solar", cfg, im.TUNE))
log = [{"try": "round 1 model", "mae": best, "kept": True}]
print(f"round 1 model: {best:.1f}")

tries = [("fleet size over a year", {"fleet_days": 365}),
         ("sun position and panel radiation", {"solar": True}),
         ("other models off", {"models": False})]
for name, change in tries:
    score = im.mae(im.backtest("solar", {**cfg, **change}, im.TUNE))
    kept = score < best
    log.append({"try": name, "mae": score, "kept": kept})
    print(f"{name}: {score:.1f} {'kept' if kept else ''}")
    if kept:
        cfg, best = {**cfg, **change}, score

pd.DataFrame(log).round(2).to_csv("results/solar_tuning.csv", index=False)
Path("results/solar_choice.json").write_text(json.dumps(cfg, indent=2))
print(json.dumps(cfg, indent=2))
