# day-ahead to imbalance: take a position on the day-ahead auction, don't deliver, settle it at the
# imbalance price. long earns imbalance - spot, short earns spot - imbalance. same steps live and in
# the backtest (scripts/06_dayahead_to_imbalance.R, scripts/08_da_imbalance.R)

library(dplyr)
library(lubridate)
library(readr)

da_cutoff_hour <- 9                 # imbalances of D-1 known up to 09:00, published about an hour later
da_fee         <- 0.055 + 0.133     # €/MWh: nord pool day-ahead fee + esett imbalance fee for denmark
price_repo     <- Sys.getenv("PRICE_REPO", "https://raw.githubusercontent.com/jacobazu/dk1-price-forecasting/master/")

# the price repo's forecast for each danish date and hour: the ensemble of bmtx, learx and xgbx
price_forecasts <- function(file = "results/backtest_forecasts.csv") {
  read_csv(paste0(price_repo, file), show_col_types = FALSE) |>
    mutate(date = as_date(date), spot_fc = (bmtx + learx + xgbx) / 3) |>
    filter(!is.na(spot_fc)) |>
    distinct(date, hour, .keep_all = TRUE) |>
    select(date, hour, spot_fc)
}

# utc start of every quarter of danish day D (92 or 100 on clock-change days)
quarter_times <- function(D) {
  tz <- "Europe/Copenhagen"
  seq(with_tz(as_datetime(paste(D, "00:00:00"), tz = tz), "UTC"),
      with_tz(as_datetime(paste(D + 1, "00:00:00"), tz = tz), "UTC") - minutes(15), by = "15 min")
}

# rows for day D as known before the auction: the spot inputs come from the price forecast, the
# imbalance shares stop at 09:00 on D-1. imb may hold D too (backtest), it is only used for scoring
da_rows <- function(D, imb, pf, weather, holidays) {
  times <- quarter_times(D)
  fc <- tibble(time = times) |>
    mutate(local = with_tz(time, "Europe/Copenhagen"), date = as_date(local), hour = hour(local)) |>
    left_join(pf, by = c("date", "hour")) |>
    transmute(time, spot = spot_fc)
  hist <- imb |>
    filter(time < times[1], time >= times[1] - days(10)) |>
    transmute(time, spot = SpotPriceEUR)
  make_quarters(imb, bind_rows(hist, fc), weather, holidays) |>
    add_recent(morning_until = da_cutoff_hour) |>
    filter(time %in% times)
}

# end of what is known for day D: 09:00 on D-1, danish time
da_until <- function(D) as_datetime(paste(D - 1, sprintf("%02d:00:00", da_cutoff_hour)), tz = "Europe/Copenhagen")

# long 1 MW if the expected spread is above the fees, short if below minus the fees, else nothing
da_position <- function(expected, fee = da_fee) ifelse(expected > fee, 1, ifelse(expected < -fee, -1, 0))

# euro per quarter for 1 MW (0.25 MWh)
da_pnl <- function(position, spread, fee = da_fee) 0.25 * (position * spread - abs(position) * fee)

# mean of a daily series with a newey-west variance (7 lags), as the dm tests in 05_evaluate.R
nw_test <- function(d, lag = 7) {
  n <- length(d)
  u <- d - mean(d)
  v <- sum(u^2) / n
  for (k in seq_len(min(lag, n - 1))) v <- v + 2 * (1 - k / (lag + 1)) * sum(u[-(1:k)] * u[1:(n - k)]) / n
  stat <- mean(d) / sqrt(v / n) * sqrt((n - 1) / n)
  c(mean = mean(d), stat = stat, p = 2 * pt(-abs(stat), df = n - 1))
}
