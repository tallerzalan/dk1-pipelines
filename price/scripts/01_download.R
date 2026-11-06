
# download dk1 day-ahead prices
# 15 min from 1 oct 2025

source("R/get_eds.R")
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)


hourly <- get_eds("Elspotprices",   "2019-01-01", "2025-10-01")
qh     <- get_eds("DayAheadPrices", "2025-10-01", format(Sys.Date() + 2))

saveRDS(hourly, "data/raw/elspot_hourly.rds")
saveRDS(qh,     "data/raw/dayahead_15min.rds")
