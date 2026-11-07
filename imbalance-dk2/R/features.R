# one row per quarter-hour with what is known at 14:00 the day before (imbalances up to 12:00)

library(dplyr)
library(tidyr)
library(lubridate)

rule_change <- as_datetime("2026-03-04 00:00:00", tz = "Europe/Copenhagen")

# imb = ImbalancePrice rows, spot = time and spot, weather = open-meteo hourly, holidays = dates
make_quarters <- function(imb, spot, weather, holidays) {
  w <- weather |>
    transmute(hour_utc = time,
              ms    = rowMeans(across(starts_with("wind_"))) / 3.6,   # km/h to m/s
              wpow  = pmin(pmax((ms - 3) / (12 - 3), 0), 1)^3,         # simple power curve
              solar = rowMeans(across(starts_with("solar_")))) |>
    select(-ms)

  spot |>
    left_join(imb |> transmute(time, dir = DominatingDirection,
                               spread = ImbalancePriceEUR - SpotPriceEUR), by = "time") |>
    mutate(local    = with_tz(time, "Europe/Copenhagen"),
           date     = as_date(local),
           hour     = hour(local),
           qh       = minute(local) %/% 15,
           wday     = wday(local, week_start = 1),
           holiday  = as.numeric(date %in% holidays),
           new_rule = as.numeric(time >= rule_change),
           hour_utc = floor_date(time, "hour")) |>
    left_join(w, by = "hour_utc") |>
    select(-hour_utc) |>
    arrange(time) |>
    group_by(date) |>
    mutate(spot_rel  = spot - mean(spot),
           spot_rank = (rank(spot) - 1) / (n() - 1),
           spot_diff = spot - lag(spot, default = first(spot))) |>
    ungroup() |>
    mutate(spot_neg = as.numeric(spot < 0))
}

# imbalance shares known at 14:00 the day before: that morning, and the same hour the week before.
# morning_until = 9 for the forecast made before the auction (R/da_imbalance.R)
add_recent <- function(q, morning_until = 12) {
  daily_hour <- q |>
    filter(!is.na(dir)) |>
    group_by(date, hour) |>
    summarise(up = mean(dir == 1), down = mean(dir == -1), .groups = "drop")

  morning <- q |>
    filter(!is.na(dir), hour < morning_until) |>
    group_by(date) |>
    summarise(morning_up = mean(dir == 1), morning_down = mean(dir == -1), .groups = "drop") |>
    mutate(date = date + 1)

  week <- bind_rows(lapply(2:8, \(k) mutate(daily_hour, date = date + k))) |>
    group_by(date, hour) |>
    summarise(week_up = mean(up), week_down = mean(down), n_days = n(), .groups = "drop") |>
    filter(n_days >= 5) |>
    select(-n_days)

  q |>
    left_join(morning, by = "date") |>
    left_join(week, by = c("date", "hour"))
}

# inputs for the direction models
feature_cols <- c("hour", "qh", "wday", "holiday", "new_rule", "spot", "spot_rel", "spot_rank",
                  "spot_diff", "spot_neg", "wpow", "solar", "morning_up", "morning_down",
                  "week_up", "week_down")

# dummies for hour, quarter and weekday, spot damped with asinh, plus the extra columns
model_matrix <- function(df, extra = c("morning_up", "morning_down", "week_up", "week_down")) {
  dummies <- function(x, levels, prefix) {
    m <- outer(x, levels[-1], "==") * 1
    colnames(m) <- paste0(prefix, levels[-1])
    m
  }
  cbind(dummies(df$hour, 0:23, "h"), dummies(df$qh, 0:3, "qh"), dummies(df$wday, 1:7, "wd"),
        as.matrix(df |> transmute(holiday, new_rule,
                                  spot = asinh(spot / 50), spot_rel = asinh(spot_rel / 50),
                                  spot_rank, spot_diff = asinh(spot_diff / 50), spot_neg,
                                  wpow, solar = solar / 100)),
        as.matrix(df[, extra]))
}
