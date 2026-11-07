# backtest for dk2: the three -x models, from 1 oct 2025 to the latest full day, as 09d_weather_de.R
# settings are dk1's (results/xgb_tuning.csv) and nothing is tuned on dk2, so this period wasn't used to choose anything

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
source("R/inputs.R")      # forecast_groups, the same list as the live forecast

prices   <- readRDS("data/processed/prices_dk2_hourly.rds")
daily    <- build_daily(prices)
features <- make_features(daily)
P        <- features$P

exog <- make_exog(daily$date, readRDS("data/raw/exog_neighbours.rds"),
                  readRDS("data/processed/holidays_dk.rds"))
exog$wx   <- make_weather(daily$date, readRDS("data/raw/weather_openmeteo.rds"))
exog$wxde <- make_weather(daily$date, read_csv("live/weather_de_live.csv", show_col_types = FALSE), "wxde")

groups <- forecast_groups
tun    <- read_csv("results/xgb_tuning.csv", show_col_types = FALSE) |> filter(model == "xgbx", chosen)
cat("xgboost settings from dk1: depth", tun$max_depth, "rounds", tun$rounds, "\n")

test_days <- which(daily$date >= as_date("2025-10-01") & complete.cases(P))
cat(length(test_days), "days,", format(daily$date[min(test_days)]), "to", format(daily$date[max(test_days)]), "\n")

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
bad <- sum(sapply(out, inherits, "try-error"))
if (bad > 0) cat(bad, "days failed in a worker\n")

actual <- tibble(date = rep(daily$date, each = 24), hour = rep(0:23, times = nrow(P)), actual = as.vector(t(P)))
yesterday <- transmute(actual, date = date + 1, hour, naive_day = actual)
week      <- transmute(actual, date = date + 7, hour, naive_week = actual)

backtest <- bind_rows(out[!sapply(out, inherits, "try-error")]) |>
  pivot_wider(names_from = model, values_from = forecast) |>
  inner_join(actual, by = c("date", "hour")) |>
  inner_join(yesterday, by = c("date", "hour")) |>
  inner_join(week, by = c("date", "hour")) |>
  # the weekly naive, as in 05_forecast_tomorrow.R: last week on mon, sat and sun
  mutate(naive_mix = if_else(wday(date, week_start = 1) %in% c(1, 6, 7), naive_week, naive_day)) |>
  select(date, hour, actual, bmtx, learx, xgbx, naive_day, naive_mix) |>
  drop_na() |>
  group_by(date) |>
  filter(n() == 24) |>
  ungroup() |>
  arrange(date, hour)

dir.create("results", showWarnings = FALSE)
write_csv(mutate(backtest, across(-c(date, hour), \(x) round(x, 2))), "results/backtest_forecasts.csv")

ens <- rowMeans(as.matrix(backtest[, c("bmtx", "learx", "xgbx")]))
cat(n_distinct(backtest$date), "full days in the backtest\n")
cat("mae ensemble", round(mean(abs(backtest$actual - ens)), 2), "naive", round(mean(abs(backtest$actual - backtest$naive_mix)), 2), "\n")

# the live files the daily run keeps up to date
wx <- readRDS("data/raw/weather_openmeteo.rds")
dir.create("live", showWarnings = FALSE)
write_csv(filter(wx, time >= as.POSIXct(Sys.Date() - 740, tz = "UTC")), "live/weather_live.csv")
write_csv(filter(readRDS("data/raw/exog_neighbours.rds"), time >= as.POSIXct(Sys.Date() - 740, tz = "UTC")), "live/neighbours_live.csv")
