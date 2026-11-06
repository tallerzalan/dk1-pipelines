# download neighbour prices and holidays

library(dplyr)
library(tidyr)
library(lubridate)
source("R/get_eds.R")
source("R/lear.R")    # lag_days()
source("R/exog.R")    # dk_holidays()

start     <- "2019-01-01"
switch_dt <- "2025-10-01"
end       <- format(Sys.Date() + 2)
areas     <- c("DE", "DK2", "NO2", "SE3", "SE4")

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# empty tibble if no data
fetch <- function(dataset, from, to, area) {
  r <- tryCatch(get_eds(dataset, from, to, area = area), error = \(e) {
    message(dataset, " ", area, ": ", conditionMessage(e)); NULL
  })
  if (is.null(r) || NROW(r) == 0) tibble() else as_tibble(r)
}


# neighbour prices
nb_old <- bind_rows(lapply(areas, \(a) fetch("Elspotprices", start, switch_dt, a)))
nb_new <- bind_rows(lapply(areas, \(a) fetch("DayAheadPrices", switch_dt, end, a)))

if (nrow(nb_old) > 0) {
  nb_old <- nb_old |>
    transmute(time = ymd_hms(HourUTC, tz = "UTC"), area = PriceArea, price = SpotPriceEUR)
}
if (nrow(nb_new) > 0) {
  nb_new <- nb_new |>
    mutate(time = floor_date(ymd_hms(TimeUTC, tz = "UTC"), "hour")) |>
    group_by(time, area = PriceArea) |>
    summarise(price = mean(DayAheadPriceEUR), n = n(), .groups = "drop") |>
    filter(n == 4) |>
    select(-n)
}

neighbours <- bind_rows(nb_old, nb_new) |>
  filter(!is.na(price)) |>
  distinct(time, area, .keep_all = TRUE) |>
  pivot_wider(names_from = area, values_from = price, names_prefix = "price_") |>
  arrange(time)


holidays <- dk_holidays(year(as_date(start)):(year(Sys.Date()) + 1))


# coverage
coverage <- function(df) {
  if (nrow(df) == 0) return(tibble())
  all_hours <- tibble(time = seq(min(df$time), max(df$time), by = "hour"))
  full <- left_join(all_hours, df, by = "time")
  bind_rows(lapply(setdiff(names(df), "time"), \(v) {
    x <- full[[v]]
    first <- which(!is.na(x))[1]
    tibble(variable    = v,
           from        = full$time[first],
           to          = full$time[max(which(!is.na(x)))],
           missing_pct = round(100 * mean(is.na(x[first:length(x)])), 2))
  }))
}

print(coverage(neighbours), n = Inf)
cat("holidays:", nrow(holidays), "days from", format(min(holidays$date)), "to", format(max(holidays$date)), "\n")

saveRDS(neighbours, "data/raw/exog_neighbours.rds")
saveRDS(holidays, "data/processed/holidays_dk.rds")
