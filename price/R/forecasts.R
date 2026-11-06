# backtest + live forecasts

library(dplyr)
library(tidyr)
library(readr)

# ensemble = mean of the three -x models
ensemble_parts <- list(ensemble = c("bmtx", "learx", "xgbx"))
add_ensembles <- function(df) {
  for (e in names(ensemble_parts)) {
    parts <- ensemble_parts[[e]]
    df[[e]] <- if (all(parts %in% names(df))) rowMeans(as.matrix(df[, parts])) else NA_real_
  }
  df
}

all_forecasts <- function(models) {
  daily  <- build_daily(readRDS("data/processed/prices_dk1_hourly.rds"))
  P      <- as.matrix(daily[, -1])
  actual <- tibble(date = rep(daily$date, each = 24), hour = rep(0:23, times = nrow(P)),
                   actual = as.vector(t(P)))

  backtest <- read_csv("results/backtest_forecasts.csv", show_col_types = FALSE) |>
    add_ensembles() |>
    select(date, hour, actual, all_of(models)) |>
    mutate(source = "backtest")

  live <- read_csv("forecasts/forecasts_live.csv", show_col_types = FALSE,
                   col_types = cols(forecast_made_utc = col_character(), date = col_date(),
                                    hour = col_integer(), .default = col_double()))
  for (m in setdiff(ensemble_parts$ensemble, names(live))) live[[m]] <- NA_real_
  live <- add_ensembles(live)
  for (m in setdiff(models, names(live))) live[[m]] <- NA_real_
  live <- live |>
    filter(date > max(backtest$date)) |>
    inner_join(actual, by = c("date", "hour")) |>
    select(date, hour, actual, all_of(models)) |>
    drop_na() |>
    group_by(date) |>
    filter(n() == 24) |>
    ungroup() |>
    mutate(source = "live")

  bind_rows(backtest, live) |>
    arrange(date, hour)
}
