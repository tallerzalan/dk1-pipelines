# DK1 wind and solar forecasting

How close can a forecast of DK1 wind and solar power, made before the noon day-ahead auction from public weather forecasts, get to what is produced? Energinet publishes its own forecast only after the auction, so a trader can't use it to bid. In Python, with the data in DuckDB.

## Setup

- **Targets:** DK1 onshore wind, offshore wind and solar per hour, from Energinet's settlement data. Three separate models.
- **When:** each forecast is made at 10:00 Danish time for the 24 hours of the next day.
- **Weather:** Open-Meteo forecasts made two days before each hour, at nine points over DK1's wind farms and solar areas. That way they were out before the auction.
- **Known output:** settled production comes 8-10 days late, so a forecast can only use output up to 10 days before the day it forecasts.
- **Fleet size:** the highest hourly output in the 60 days up to that point, per target. The models forecast output as a share of it, so new parks (such as Thor offshore) don't break them.

## Rules, set before any model is scored

- **Backtest:** from March 2025 to the latest settled day. The models are refit on the first of each month on all data up to 10 days before.
- **Models:** a power curve (average share of the fleet in each bin of forecast wind speed or radiation), LightGBM on all weather points plus hour and day of the year, and the average of the two.
- **Score:** mean absolute error in MW, and as a share of the fleet. All hours count, including hours where parks turned down at negative prices.
- **Worth it:** LightGBM is worth it if its daily mean absolute error is lower than the power curve's, Diebold-Mariano with Newey-West, p < 0.05.
- **Energinet:** its day-ahead forecast is reported but isn't pass/fail. It uses later weather, and it doesn't seem to cover all of DK1's offshore fleet.

## Trying to beat Energinet, set 21 Oct 2026 before trying

The first backtest beat Energinet on offshore but not on onshore or solar. To improve without fishing for a lucky result:

- **Two periods.** March to December 2025 is for trying things. January 2026 to the latest settled day is not looked at until the end, and the final model is run on it once. (The first backtest above already covered 2026, but only with the first model.)
- **What is tried, one at a time, each kept only if it lowers the error on 2025:**
  1. Fresher weather: ECMWF's 00 UTC run from the day before (ready around 07 UTC), instead of a forecast made two days before.
  2. More weather models as extra inputs: DWD ICON-EU, DMI HARMONIE, MET Norway Nordic and NOAA GFS. (Open-Meteo has no archived forecasts from two days before for DMI HARMONIE or MET Nordic, so only ICON-EU and GFS were used.)
  3. More features: the spread of wind speed over the points, gusts, and for solar the sun's height and direct radiation.
  4. LightGBM settings, chosen on 2025 only.
- **Kept or not:** an addition is kept for a target if it lowers that target's mean absolute error on 2025.
- **Compared with Energinet on 2026:** as published, and scaled to the whole fleet with the ratio of output to its forecast over the 60 days up to 10 days before. A target counts as beaten if the mean absolute error is lower than both, and the daily errors pass Diebold-Mariano with Newey-West, p < 0.05.

## Result on 2026, run once on 22 Oct 2026

January to 11 October 2026, mean absolute error in MW. Kept on 2025: ECMWF's fresher run and ICON-EU and GFS as extra inputs for all three, plus the extra features for offshore.

| | Ours | Energinet | Energinet scaled to the fleet | Beaten |
|---|---|---|---|---|
| Onshore | 180 | 194 | 154 | no |
| Offshore | 170 | 360 | 210 | yes, p < 0.001 against both |
| Solar | 120 | 97 | 94 | no |

- **Offshore:** clearly better, every month of 2026. Energinet's forecast seems to miss part of the fleet, and even scaled up it is 20% behind.
- **Onshore:** lower than Energinet's published forecast but not significantly (p = 0.06), and 17% behind it scaled up. Energinet knows every wind farm and forecasts later.
- **Solar:** behind. Most of the gap is in spring and summer. The fleet size (the highest hour in the last 60 days) follows the season for solar, not only new panels, so it lags in spring. A fleet measure over a whole year would suit solar better. That is for a later round, scored on new days.

## Round 2 for solar, set 22 Oct 2026 before trying

2026 has been used, so it can't judge a new solar model. Instead:

- **Tried on March to December 2025, one at a time, kept if the error there drops:** fleet size as the highest hour in the past year instead of 60 days; the sun's height and direction, ECMWF's direct and diffuse radiation, and the radiation on a panel tilted 35 degrees to the south; and leaving out the other weather models.
- **Verdict from live days:** the chosen model forecasts every day before the auction from when the daily run starts. It is judged on 1 April to 30 June 2027 against Energinet as published and scaled, with the same rule as above: lower error than both, Diebold-Mariano p < 0.05.

## License

Code: [MIT](LICENSE). Data from [Energi Data Service](https://www.energidataservice.dk) (Energinet) and [Open-Meteo.com](https://open-meteo.com) ([CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)).
