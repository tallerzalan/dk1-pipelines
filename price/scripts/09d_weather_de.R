# -x models with german weather too, vs 09b_weather.R. rule set before: use it if mae drops with p < 0.05
# result: ensemble mae 19.05 -> 16.98 (-10.9%, p < 0.001), used from the forecast for 14 oct 2026

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
source("R/lear.R")
source("R/bmt.R")
source("R/exog.R")
source("R/xmodels.R")
source("R/xgb.R")
source("R/parallel.R")
source("R/tests.R")
source("R/inputs.R")      # forecast_groups, the same list as the live forecast

prices  <- readRDS("data/processed/prices_dk1_hourly.rds")
daily    <- build_daily(prices)
features <- make_features(daily)
P        <- features$P

exog <- make_exog(daily$date, readRDS("data/raw/exog_neighbours.rds"),
                  readRDS("data/processed/holidays_dk.rds"))
exog$wx   <- make_weather(daily$date, readRDS("data/raw/weather_openmeteo.rds"))
exog$wxde <- make_weather(daily$date, read_csv("live/weather_de_live.csv", show_col_types = FALSE), "wxde")

groups <- forecast_groups
tun   <- read_csv("results/xgb_tuning.csv", show_col_types = FALSE) |> filter(model == "xgbx", chosen)

test_days <- which(daily$date >= as_date("2025-10-01") & complete.cases(P))

run_day <- function(d) {
  f <- list(
    bmtx  = tryCatch(bmtx_forecast_day(features, exog, d, groups), error = \(e) NULL),
    learx = tryCatch(learx_forecast_day(features, exog, d, groups), error = \(e) NULL),
    xgbx  = tryCatch(xgb_forecast_day(features, d, exog = exog[groups],
                                      params = modifyList(xgb_params, list(max_depth = tun$max_depth)),
                                      rounds = tun$rounds, seed = as.integer(daily$date[d])),
                     error = \(e) NULL))
  bind_rows(lapply(names(f), \(m) if (is.null(f[[m]])) NULL else
    tibble(date = daily$date[d], hour = 0:23, model = m, forecast = f[[m]])))
}

t0 <- Sys.time()
out <- par_lapply(test_days, run_day)
cat("done in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")

actual <- tibble(date = rep(daily$date, each = 24), hour = rep(0:23, times = nrow(P)),
                 actual = as.vector(t(P)))
fc <- bind_rows(out) |> inner_join(actual, by = c("date", "hour"))
saveRDS(fc, "data/processed/forecasts_weather_de.rds")

# with vs without, same hours, ensemble = average of the three
with_ens <- function(x) {
  bind_rows(x, x |> group_by(date, hour, actual) |>
              summarise(forecast = mean(forecast), n = n(), .groups = "drop") |>
              filter(n == 3) |> select(-n) |> mutate(model = "ensemble"))
}
both <- inner_join(with_ens(readRDS("data/processed/forecasts_weather.rds")) |> rename(without = forecast),
                   with_ens(fc) |> rename(with = forecast), by = c("date", "hour", "model", "actual"))
both |>
  group_by(model, date) |>
  summarise(without = mean(abs(actual - without)), with = mean(abs(actual - with)), .groups = "drop") |>
  group_by(model) |>
  summarise(days = n(), p = dm_test(with - without)[["p"]], mae_without = mean(without), mae_with = mean(with),
            change = sprintf("%+.1f%%", 100 * (mean(with) / mean(without) - 1)), .groups = "drop") |>
  mutate(across(c(mae_without, mae_with), \(v) round(v, 2)), p = signif(p, 2)) |>
  print(n = Inf)
