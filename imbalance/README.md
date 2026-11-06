# DK1 imbalance forecasting (WIP)

Forecasts of the imbalance in DK1, in R. For each quarter-hour it gives the chance that Energinet regulates up, down or not at all, and the spread between the imbalance price and the spot price.

Dashboard: [jacobazu.github.io/dk1-imbalance-forecasting](https://jacobazu.github.io/dk1-imbalance-forecasting)

Methods: [one page (PDF)](notes/method.pdf)

Models: multinomial lasso, XGBoost and their average. The benchmark is how often each direction happened in that hour over the last 28 days.

## The real test

A backtest shows that a method would have worked. It doesn't show that it works. I made my choices with the backtest period in view, so the backtest is probably a bit too good. The live forecasts are saved before their quarter starts, so they can't be adjusted afterwards. That's the interesting part, so the dashboard's main page only shows the live forecasts and how they score. The backtest has its own page.

## What the backtest shows

- The next hour is fairly predictable: 4.9% lower log loss than the benchmark, and picking a side with the spread forecast earns 12.5 €/MWh (p < 0.001). Imbalances tend to last, and the real-time state of the grid shows it.
- Tomorrow only a little: 1.5% lower log loss, and picking a side earns 1.1 €/MWh, which isn't significant (p = 0.28). If tomorrow's imbalance were easy to see, the market would already have adjusted for it.
- Before the auction it is about the same, 1.4% lower log loss, and the day-ahead to imbalance test below doesn't pass its rule.
- So the edge is in the next hour, which is traded on the intraday market. Intraday prices aren't public, so spot stands in for them and the real value can't be measured here.

## Day-ahead to imbalance (a paper test)

A friend of mine pointed out that a day-ahead position without physical power is settled at the imbalance price, and I took it from there. Buying a quarter on day-ahead then earns the imbalance price minus spot, and selling it earns spot minus the imbalance price. So the question is whether the spread can be forecast well enough before the noon auction to be worth a position.

- Every morning between 10:30 and 11:55 Danish time (`scripts/08_da_imbalance.R`): imbalances up to 09:00, the [price forecast](https://github.com/jacobazu/dk1-price-forecasting) in place of the spot price, which isn't known yet, and weather made 48 hours ahead. Long 1 MW in each quarter where the expected spread is above the fees, short where it is below minus the fees. The positions are saved before the auction and scored once the imbalance prices are out.
- Fees: Nord Pool's day-ahead fee, 0.055 €/MWh ([fee schedule 2026](https://www.nordpoolgroup.com/4a7ad8/globalassets/trading-and-services/nord-pool-fee-schedule-2026-nordic-and-baltic-market.pdf)), and eSett's imbalance fee for Denmark, 0.133 €/MWh ([eSett, Oct 2026](https://www.esett.com/app/uploads/2026/08/Imbalance-Settlement-Agreement-Appendix-1-1.10.2026-31.10.2026.pdf)). The yearly and weekly fees are left out.
- The rule was set and committed before the backtest was run (`scripts/06_dayahead_to_imbalance.R`): worth trading only if the mean daily P&L is above 0 with p < 0.05 and it beats the benchmark forecast, always long and always short with p < 0.05 (Holm).
- Backtest, 2 Nov 2025 to 10 Oct 2026: +37 € a day with 1 MW (p = 0.13), and the gains over the alternatives aren't significant either. So by that rule it isn't worth trading. It runs live as a paper test anyway.
- A test, not trading. Energinet expects a balance responsible party to aim to be in balance ([Energinet, 2021](https://forsyningstilsynet.dk/Media/638215639738153946/2021_09_17_bilag1.pdf)), so whether this may be done on purpose depends on the market rules and the party's agreement. Closing a position on the intraday market isn't modelled, because intraday prices aren't public.

## Why these tests

- Diebold-Mariano with Newey-West: is the gain real or luck?
- Calibration: when it says 40%, does it happen about 40% of the time?
- Value of picking a side: is the forecast worth anything?

## License

Code: [MIT](LICENSE). Not trading advice.

Data from [Energi Data Service](https://www.energidataservice.dk) (Energinet), used under "Conditions for use of Danish public-sector data". Not covered by the MIT License or endorsed by Energinet. Weather from [Open-Meteo.com](https://open-meteo.com), [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
