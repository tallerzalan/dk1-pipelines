# Analysis (hindsight only)

**Never a forecast input.** Everything here uses data published after the auction. The forecast lives in `scripts/` and `R/` and never reads from this folder; the dashboard won't build if it does.

It is here to explain prices and forecast errors.

- `01_download_entsoe.R`: actual generation by type and actual load for DK1 and Germany, hourly. Needs `ENTSOE_TOKEN`.

First question: why are the forecasts too low at 18-21 on expensive days?
