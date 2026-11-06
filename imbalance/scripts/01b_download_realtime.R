# download real-time data (5-minute averages) and energinet's wind/solar forecasts

library(dplyr)
library(tidyr)
library(lubridate)
source("R/get_eds.R")

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

start <- as_date("2025-03-01")
end   <- Sys.Date() + 1
cols  <- c("Minutes1UTC", "aFRR_ActivatedDK1", "OffshoreWindPower", "OnshoreWindPower", "SolarPower",
           "Exchange_DK1_DE", "Exchange_DK1_NO", "Exchange_DK1_SE", "Exchange_DK1_NL", "Exchange_DK1_DK2")

weeks <- seq(start, end, by = "week")
rt <- bind_rows(lapply(weeks, \(w) {
  message("right now ", w)
  Sys.sleep(1)
  get_eds("PowerSystemRightNow", format(w), format(min(w + 7, end)), area = NULL, columns = cols)
})) |>
  mutate(time = floor_date(ymd_hms(Minutes1UTC, tz = "UTC"), "5 min")) |>
  select(-Minutes1UTC) |>
  group_by(time) |>
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)), n_min = n(), .groups = "drop") |>
  mutate(across(-c(time, n_min), \(x) if_else(is.nan(x), NA_real_, x)))
saveRDS(rt, "data/raw/realtime.rds")

# energinet wind and solar forecasts, dk1 and dk2
fc <- bind_rows(lapply(seq(start, end, by = "month"), \(m) {
  message("forecasts ", m)
  Sys.sleep(1)
  get_eds("Forecasts_Hour", format(m), format(min(m %m+% months(1), end)), area = c("DK1", "DK2"))
}))
saveRDS(fc, "data/raw/energinet_forecasts.rds")
