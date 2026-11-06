# DK1 pipelines

Runs two DK1 forecasting projects on a schedule, so a dashboard can read their results without depending on the original
repositories staying active.

| Folder | What it does | Workflow | Original |
|---|---|---|---|
| `price/` | Forecasts tomorrow's hourly day-ahead prices in DK1, once a day before the noon auction | `price.yml` | [jacobazu/dk1-price-forecasting](https://github.com/jacobazu/dk1-price-forecasting) |
| `imbalance/` | Forecasts DK1 imbalance direction and spread every hour | `imbalance-live.yml` | [jacobazu/dk1-imbalance-forecasting](https://github.com/jacobazu/dk1-imbalance-forecasting) |
| `imbalance/` | A day-ahead to imbalance paper test: simulated positions, nothing is traded | `imbalance-dayahead.yml` | same |

All the forecasting work and method here is by the original author, jacobazu. This is a copy that runs by itself, not the
original project and not endorsed by its author.

## What differs from the originals

The code in `price/` and `imbalance/` is unchanged, copied from these commits:

- `price/`: d492a90d7df63b52f97191d6a9aaa50dc3063cdd
- `imbalance/`: c0d1c4a0bbe780966e024d31109c1bba23ce501d

Only the workflows differ: they run from the two folders, build no dashboard or GitHub Pages site, try more often because
GitHub drops many scheduled runs, and read the price forecast from this repository. The optional ENTSO-E timing check is off.

Everything in the logs up to those commits was made by the original project. Rows made after that were made here.

## Licence and data

Code: MIT, see the `LICENSE` in each folder. Not trading advice.

Data from [Energi Data Service](https://www.energidataservice.dk) (Energinet), used under "Conditions for use of Danish
public-sector data". Not covered by the MIT License or endorsed by Energinet. Weather from
[Open-Meteo.com](https://open-meteo.com), [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
