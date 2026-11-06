# download imbalance prices (from 4 mar 2025) and weather forecasts

library(dplyr)
library(lubridate)
source("R/get_eds.R")
source("R/weather.R")

start <- as_date("2025-03-04")
end   <- Sys.Date() + 1
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# imbalance prices, one month at a time
months <- seq(floor_date(start, "month"), end, by = "month")
imb <- bind_rows(lapply(months, \(m) {
  message("imbalance ", format(m, "%Y-%m"))
  Sys.sleep(1)
  get_eds("ImbalancePrice", format(max(m, start)), format(min(m %m+% months(1), end)))
})) |>
  mutate(time = ymd_hms(TimeUTC, tz = "UTC")) |>
  distinct(time, .keep_all = TRUE) |>
  arrange(time)

saveRDS(imb, "data/raw/imbalance.rds")

# weather forecasts made 48 hours ahead
weather <- get_weather(start - 60, end)
saveRDS(weather, "data/raw/weather.rds")
cat("weather:", format(min(weather$time)), "to", format(max(weather$time)), "\n")

