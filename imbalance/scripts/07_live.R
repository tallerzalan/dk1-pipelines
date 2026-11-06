# live run every hour: next 4 hours, and tomorrow after the auction. logs are append-only
# LOCAL=1 uses data/raw, NOW = time to run as (for testing)

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
source("R/intraday.R")
source("R/spread.R")
source("R/live.R")

now    <- if (nzchar(Sys.getenv("NOW"))) ymd_hm(Sys.getenv("NOW"), tz = "UTC") else Sys.time()
now_dk <- with_tz(now, "Europe/Copenhagen")
issue  <- floor_date(now, "hour")
local  <- Sys.getenv("LOCAL") == "1"

# local test: downloaded files cut at `now`
local_data <- function(days) {
  from <- now - days(days)
  list(imb     = readRDS("data/raw/imbalance.rds") |> filter(time >= from, time < now - minutes(30)),
       spot    = readRDS("data/raw/spot.rds") |> filter(time >= from),
       rt      = readRDS("data/raw/realtime.rds") |> filter(time >= from, time < now - minutes(10)),
       fc      = readRDS("data/raw/energinet_forecasts.rds"),
       weather = readRDS("data/raw/weather.rds"))
}
get_data <- function(days) if (local) local_data(days) else get_live_data(now, days)

# models, refit once a week on the year before
dir.create("models", showWarnings = FALSE)
models_file <- "models/models.rds"
models <- if (file.exists(models_file)) readRDS(models_file) else NULL
if (is.null(models) || difftime(now, models$fitted_at, units = "days") >= 7) {
  message("refitting the models")
  refit <- tryCatch(fit_models(get_data(400), issue), error = \(e) {
    if (is.null(models)) stop(e)
    message("refit failed, keeping the models from ", format(models$fitted_at), ": ", conditionMessage(e))
    NULL
  })
  if (!is.null(refit)) {
    models <- refit
    saveRDS(models, models_file)
  }
}

data <- get_data(62)
dir.create("forecasts", showWarnings = FALSE)

# append to a log, existing rows are never changed
append_log <- function(new, file, keys) {
  new <- mutate(new, made_utc = format(now, "%Y-%m-%d %H:%M", tz = "UTC"), .before = 1) |>
    mutate(across(where(\(x) is.double(x) && !inherits(x, c("POSIXt", "Date"))), \(x) round(x, 4)))
  if (file.exists(file)) {
    old <- read_csv(file, show_col_types = FALSE, col_types = cols(made_utc = col_character()))
    new <- anti_join(new, old, by = keys)
    new <- bind_rows(old, new)
  }
  write_csv(new, file)
}

# next 4 hours, only quarters that have not started yet
fi <- forecast_intraday(models, data, issue)
if (is.null(fi)) {
  cat("intraday: no forecast, the quarter an hour ago has missing data\n")
} else {
  fi <- filter(fi, time > now)
  append_log(fi, "forecasts/intraday_log.csv", c("issue", "time", "model"))
  cat("intraday:", nrow(fi) / 2, "quarters from", format(with_tz(issue, "Europe/Copenhagen")), "danish time\n")
}

# tomorrow, once the auction is done
tomorrow <- as_date(now_dk) + 1
n_spot   <- sum(as_date(with_tz(data$spot$time, "Europe/Copenhagen")) == tomorrow)
done     <- file.exists("forecasts/dayahead_log.csv") &&
  tomorrow %in% as_date(read_csv("forecasts/dayahead_log.csv", show_col_types = FALSE)$date)
if (n_spot >= 92 && !done) {
  fd <- forecast_dayahead(models, data, tomorrow)
  append_log(fd, "forecasts/dayahead_log.csv", c("date", "time", "model"))
  cat("day-ahead forecast for", format(tomorrow), "\n")
}

# score forecasts whose quarter is now known
actual <- data$imb |> filter(!is.na(DominatingDirection)) |> transmute(time, dir = DominatingDirection)
score <- function(log, horizon_of) {
  x <- read_csv(log, show_col_types = FALSE, col_types = cols(made_utc = col_character()))
  x$horizon <- horizon_of(x)
  x |>
    inner_join(actual, by = "time") |>
    mutate(ll = log_loss(cbind(down, none, up), dir)) |>
    select(time, horizon, model, ll, dir) |>
    pivot_wider(names_from = model, values_from = ll, names_prefix = "ll_")
}
scored <- bind_rows(
  if (file.exists("forecasts/intraday_log.csv"))
    score("forecasts/intraday_log.csv", \(x) paste0(x$bucket - 1, "-", x$bucket, "h")),
  if (file.exists("forecasts/dayahead_log.csv"))
    score("forecasts/dayahead_log.csv", \(x) rep("day-ahead", nrow(x))))
if (nrow(scored) > 0) {
  dir.create("results", showWarnings = FALSE)
  append_log(scored |> select(-dir), "results/live_scored.csv", c("time", "horizon"))
}

# what happened in each quarter, for the dashboard's last 24 hours
outcomes <- data$imb |>
  filter(!is.na(DominatingDirection), !is.na(ImbalancePriceEUR), !is.na(SpotPriceEUR)) |>
  transmute(time, dir = DominatingDirection, spread = ImbalancePriceEUR - SpotPriceEUR)
if (nrow(outcomes) > 0) {
  dir.create("results", showWarnings = FALSE)
  append_log(outcomes, "results/outcomes.csv", "time")
}
