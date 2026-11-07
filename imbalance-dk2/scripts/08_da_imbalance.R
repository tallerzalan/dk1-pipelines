# day-ahead to imbalance, live paper trading. between 10:30 and 11:55 on D-1 (after the price forecast,
# before the noon auction): a position for every quarter of D, same steps as the backtest
# (06_dayahead_to_imbalance.R). every run: P&L for the days whose imbalance prices are all out.
# logs are append-only. LOCAL=1 uses data/raw, NOW = time to run as (for testing)

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(xgboost)
source("R/get_eds.R")
source("R/weather.R")
source("R/download.R")
source("R/holidays.R")
source("R/features.R")
source("R/direction.R")
source("R/spread.R")
source("R/da_imbalance.R")

now    <- if (nzchar(Sys.getenv("NOW"))) ymd_hm(Sys.getenv("NOW"), tz = "UTC") else Sys.time()
now_dk <- with_tz(now, "Europe/Copenhagen")
local  <- Sys.getenv("LOCAL") == "1"
target <- as_date(now_dk) + 1
hol    <- dk_holidays(2025:2030)$date

# imbalances published so far and weather made 48 hours ahead
get_data <- function(days) {
  today <- as_date(now_dk)
  if (local) {
    list(imb     = readRDS("data/raw/imbalance.rds") |> filter(time >= now - days(days), time < now - minutes(60)),
         weather = readRDS("data/raw/weather.rds"))
  } else {
    list(imb     = get_imbalance(today - days, today + 1),
         weather = if (days > 62) get_weather(today - days, today + 1) else get_weather_saved(today - days, today + 1))
  }
}
history <- function(data) {
  make_quarters(data$imb, transmute(data$imb, time, spot = SpotPriceEUR), data$weather, hol) |>
    add_recent(morning_until = da_cutoff_hour)
}

# models, refit once a week on the year before, as in the backtest
dir.create("models", showWarnings = FALSE)
models_file <- "models/da_models.rds"
models <- if (file.exists(models_file)) readRDS(models_file) else NULL
if (is.null(models) || difftime(now, models$fitted_at, units = "days") >= 7) {
  message("refitting the models")
  q <- history(get_data(400))
  day <- as_date(now_dk)
  train <- q[complete.cases(q[, feature_cols]) & !is.na(q$dir) & q$date >= day - 365 & q$date < day, ]
  models <- list(fitted_at = now, mnl = fit_mnl(train), xgb = fit_xgb(train, as.integer(day)))
  saveRDS(models, models_file)
}

data <- get_data(62)
q    <- history(data)
dir.create("forecasts", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)
log_file <- "forecasts/da_imbalance_log.csv"
read_log <- function() read_csv(log_file, show_col_types = FALSE, col_types = cols(made_utc = col_character()))

# positions for tomorrow, only before the auction and once the price forecast is out
in_window <- now_dk >= as_datetime(paste(as_date(now_dk), "10:30:00"), tz = "Europe/Copenhagen") &&
  now_dk < as_datetime(paste(as_date(now_dk), "11:55:00"), tz = "Europe/Copenhagen")
done <- file.exists(log_file) && target %in% as_date(read_log()$date)
if (!in_window) {
  message("Outside 10:30-11:55 Danish time, no position for ", target)
} else if (done) {
  message("Positions for ", target, " already saved")
} else {
  pf   <- price_forecasts("forecasts/forecasts_live.csv")
  test <- da_rows(target, data$imb, pf, data$weather, hol)
  test <- test[complete.cases(test[, feature_cols]), ]
  if (nrow(test) != length(quarter_times(target))) {
    message("No complete price forecast or inputs for ", target, " yet, no position")
  } else {
    until <- da_until(target)
    known <- q |> filter(!is.na(dir), !is.na(spread), time < until)
    P <- list(clim = clim_probs(known[known$date >= target - 30, ], test),
              mnl  = pred_mnl(models$mnl, test), xgb = pred_xgb(models$xgb, test))
    P$ens <- (P$mnl + P$xgb) / 2
    pools   <- spread_pools(known, until)
    near_dn <- near_spot(pools$down, test$spot)
    near_up <- near_spot(pools$up, test$spot)
    new <- bind_rows(lapply(c("ens", "clim"), \(m) {
      expected <- mixture_quantiles(P[[m]], near_dn, near_up)[, "mean"]
      tibble(made_utc = format(now, "%Y-%m-%d %H:%M", tz = "UTC"), date = target, time = test$time, model = m,
             spot_fc = round(test$spot, 2), as_tibble(round(P[[m]], 4)), expected = round(expected, 2),
             position = da_position(expected))
    }))
    if (file.exists(log_file)) new <- bind_rows(mutate(read_log(), date = as_date(date)), new)
    write_csv(new, log_file)
    pos <- new$position[new$date == target & new$model == "ens"]
    cat("positions for", format(target), ":", sum(pos == 1), "long,", sum(pos == -1), "short,",
        sum(pos == 0), "none\n")
  }
}

# P&L for days whose imbalance prices are all out: the forecast, the benchmark, always long or short
if (file.exists(log_file)) {
  out_file <- "results/da_imbalance_live.csv"
  old  <- if (file.exists(out_file)) read_csv(out_file, show_col_types = FALSE) else NULL
  lg   <- read_log() |> mutate(date = as_date(date))
  days <- unique(lg$date)
  days <- days[!days %in% old$date]
  scored <- bind_rows(lapply(days, \(D) {
    act <- q |> filter(date == D, !is.na(spread)) |> select(time, spread)
    if (nrow(act) != length(quarter_times(D))) return(NULL)
    x <- lg |> filter(date == D) |> select(time, model, position) |>
      pivot_wider(names_from = model, values_from = position) |>
      inner_join(act, by = "time")
    tibble(date = D, quarters = nrow(x),
           ens = sum(da_pnl(x$ens, x$spread)), clim = sum(da_pnl(x$clim, x$spread)),
           long = sum(da_pnl(1, x$spread)), short = sum(da_pnl(-1, x$spread)),
           perfect = sum(da_pnl(sign(x$spread), x$spread)))
  }))
  if (nrow(scored) > 0) {
    write_csv(bind_rows(old, mutate(scored, across(-c(date, quarters), \(v) round(v, 2)))) |> arrange(date), out_file)
    cat("scored", nrow(scored), "day(s)\n")
  }
}
