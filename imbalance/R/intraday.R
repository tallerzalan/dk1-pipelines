# intraday: every hour, the 16 quarters of the next 4 hours, last known quarter started 60 min before

library(dplyr)
library(tidyr)
library(lubridate)

# system state per quarter, used as the last known quarter
make_state <- function(q, imb) {
  grid <- tibble(time = seq(min(q$time), max(q$time), by = "15 min"))
  x <- grid |>
    left_join(q |> select(time, dir, spread), by = "time") |>
    left_join(imb |> transmute(time, sd = SatisfiedDemand, afrr_up = aFRRUpMW,
                               afrr_down = aFRRDownMW), by = "time")
  roll <- function(v, k) as.numeric(stats::filter(v, rep(1 / k, k), sides = 1))
  x |>
    transmute(last_time  = time,
              last_down  = as.numeric(dir == -1), last_none = as.numeric(dir == 0),
              last_up    = as.numeric(dir == 1),
              up_1h      = roll(dir == 1, 4),  down_1h = roll(dir == -1, 4),
              up_4h      = roll(dir == 1, 16), down_4h = roll(dir == -1, 16),
              sd_last    = asinh(sd / 100), sd_1h = asinh(roll(sd, 4) / 100),
              afrr_up_1h = roll(afrr_up, 4) / 10, afrr_down_1h = roll(afrr_down, 4) / 10,
              spread_last = asinh(spread / 50), spread_1h = asinh(roll(abs(spread), 4) / 50))
}
state_cols <- c("last_down", "last_up", "up_1h", "down_1h", "up_4h", "down_4h", "sd_last", "sd_1h",
                "afrr_up_1h", "afrr_down_1h", "spread_last", "spread_1h")

# one row per issue hour and target quarter, lead = quarters after the last known, bucket = hours ahead
make_issues <- function(q, state, issue_times) {
  crossing(issue = issue_times, k = 0:15) |>
    mutate(time = issue + minutes(15 * k), last_time = issue - minutes(60), lead = k + 4,
           bucket = k %/% 4 + 1, lead_in_hour = k %% 4) |>
    select(-k) |>
    inner_join(q, by = "time") |>
    inner_join(state, by = "last_time") |>
    mutate(down_x_lead = last_down * lead_in_hour, up_x_lead = last_up * lead_in_hour)
}
intraday_cols <- c(state_cols, "lead_in_hour", "down_x_lead", "up_x_lead")

# real-time state up to 10 min before each issue time, errors = actual wind/solar minus energinet's forecast
make_realtime <- function(rt, fc, issue_times) {
  da <- fc |>
    transmute(hour = ymd_hms(HourUTC, tz = "UTC"), type = ForecastType, da = ForecastDayAhead) |>
    group_by(hour, type) |>
    summarise(da = sum(da), .groups = "drop") |>
    pivot_wider(names_from = type, values_from = da)
  x <- rt |>
    mutate(hour = floor_date(time, "hour")) |>
    left_join(da, by = "hour") |>
    transmute(time, afrr = aFRR_ActivatedDK1,
              wind_err  = OffshoreWindPower + OnshoreWindPower - `Offshore Wind` - `Onshore Wind`,
              solar_err = SolarPower - Solar,
              flow_de = Exchange_DK1_DE, flow_no = Exchange_DK1_NO, flow_se = Exchange_DK1_SE,
              flow_nl = Exchange_DK1_NL, flow_dk2 = Exchange_DK1_DK2) |>
    arrange(time)
  # mean of the last k 5-minute values
  win <- function(v, k) {
    idx <- findInterval(as.numeric(issue_times - minutes(10)), as.numeric(x$time))
    cs <- c(0, cumsum(ifelse(is.na(v), 0, v))); cn <- c(0, cumsum(!is.na(v)))
    lo <- pmax(idx - k, 0)
    out <- (cs[idx + 1] - cs[lo + 1]) / (cn[idx + 1] - cn[lo + 1])
    out[!is.finite(out)] <- NA
    out
  }
  tibble(issue     = issue_times,
         afrr_15   = asinh(win(x$afrr, 3) / 50),  afrr_60 = asinh(win(x$afrr, 12) / 50),
         wind_err  = win(x$wind_err, 12) / 1000,  solar_err = win(x$solar_err, 12) / 1000,
         flow_de   = win(x$flow_de, 12) / 1000,   flow_no = win(x$flow_no, 12) / 1000,
         flow_se   = win(x$flow_se, 12) / 1000,   flow_nl = win(x$flow_nl, 12) / 1000,
         flow_dk2  = win(x$flow_dk2, 12) / 1000)
}
realtime_cols <- c("afrr_15", "afrr_60", "wind_err", "solar_err", "flow_de", "flow_no", "flow_se",
                   "flow_nl", "flow_dk2")
