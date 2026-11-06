# downloads for the live run

library(dplyr)
library(tidyr)
library(lubridate)

# small chunks per request
chunks <- function(from, to, by) {
  s <- seq(as_date(from), as_date(to), by = by)
  tibble(from = s, to = pmin(lead(s, default = as_date(to) + 1), as_date(to) + 1))
}

get_imbalance <- function(from, to) {
  ch <- chunks(from, to, "month")
  bind_rows(lapply(seq_len(nrow(ch)), \(i) {
    Sys.sleep(0.5)
    get_eds("ImbalancePrice", format(ch$from[i]), format(ch$to[i]))
  })) |>
    mutate(time = ymd_hms(TimeUTC, tz = "UTC")) |>
    distinct(time, .keep_all = TRUE) |>
    arrange(time)
}

# power system right now, minute data as 5-minute averages
rt_cols <- c("Minutes1UTC", "aFRR_ActivatedDK1", "OffshoreWindPower", "OnshoreWindPower", "SolarPower",
             "Exchange_DK1_DE", "Exchange_DK1_NO", "Exchange_DK1_SE", "Exchange_DK1_NL", "Exchange_DK1_DK2")
get_realtime <- function(from, to) {
  ch <- chunks(from, to, "week")
  bind_rows(lapply(seq_len(nrow(ch)), \(i) {
    Sys.sleep(0.5)
    get_eds("PowerSystemRightNow", format(ch$from[i]), format(ch$to[i]), area = NULL, columns = rt_cols)
  })) |>
    mutate(time = floor_date(ymd_hms(Minutes1UTC, tz = "UTC"), "5 min")) |>
    select(-Minutes1UTC) |>
    group_by(time) |>
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE)), n_min = n(), .groups = "drop") |>
    mutate(across(-c(time, n_min), \(x) if_else(is.nan(x), NA_real_, x)))
}

# energinet's wind and solar forecasts, dk1 and dk2
get_energinet_forecasts <- function(from, to) {
  ch <- chunks(from, to, "month")
  bind_rows(lapply(seq_len(nrow(ch)), \(i) {
    Sys.sleep(0.5)
    get_eds("Forecasts_Hour", format(ch$from[i]), format(ch$to[i]), area = c("DK1", "DK2"))
  }))
}

# day-ahead prices per quarter
get_spot <- function(from, to) {
  get_eds("DayAheadPrices", format(as_date(from)), format(as_date(to) + 1)) |>
    transmute(time = ymd_hms(TimeUTC, tz = "UTC"), spot = DayAheadPriceEUR) |>
    distinct(time, .keep_all = TRUE) |>
    arrange(time)
}

# open-meteo is sometimes down for a quarter of an hour or more. forecasts made two days before don't
# change, so the last download is kept in the repository and only the last few days are fetched again
weather_file <- "live/weather.csv"
get_weather_saved <- function(from, to) {
  old <- if (file.exists(weather_file)) readr::read_csv(weather_file, show_col_types = FALSE) else NULL
  start <- if (is.null(old)) as_date(from) else max(as_date(from), as_date(max(old$time)) - 3)
  new <- tryCatch(get_weather(start, to), error = \(e) {
    message("open-meteo failed, using the saved weather: ", conditionMessage(e))
    NULL
  })
  if (is.null(new) && is.null(old)) stop("no weather: open-meteo failed and nothing is saved")
  w <- bind_rows(new, old) |>
    distinct(time, .keep_all = TRUE) |>
    filter(time >= as_datetime(as_date(from))) |>
    arrange(time)
  if (!is.null(new)) {
    dir.create("live", showWarnings = FALSE)
    readr::write_csv(w, weather_file)
  }
  w
}

# everything for one run, days back: 62 to forecast, 400 to refit
get_live_data <- function(now, days) {
  today <- as_date(with_tz(now, "Europe/Copenhagen"))
  imb <- get_imbalance(today - days, today + 1)
  spot_new <- get_spot(today - 1, today + 1)
  list(imb     = imb,
       spot    = bind_rows(spot_new, transmute(imb, time, spot = SpotPriceEUR)) |>
                   distinct(time, .keep_all = TRUE) |> arrange(time),
       rt      = get_realtime(today - if (days > 62) days else 1, today + 1),
       fc      = get_energinet_forecasts(today - min(days, 400), today + 2),
       weather = if (days > 62) get_weather(today - days, today + 1) else get_weather_saved(today - days, today + 1))
}
