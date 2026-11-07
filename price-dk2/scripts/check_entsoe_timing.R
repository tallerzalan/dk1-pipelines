# timing check only, not a model input: are tomorrow's day-ahead wind/solar forecasts
# from the tsos on entso-e already out when the forecast runs (~10:15)? one row per run in live/entsoe_timing.csv

library(httr2)
library(dplyr)
library(lubridate)
library(readr)

areas <- c(DK1 = "10YDK-1--------W", DE = "10Y1001A1001A82H")
types <- c(B16 = "solar", B18 = "wind_offshore", B19 = "wind_onshore")

now    <- Sys.time()
target <- as_date(with_tz(now, "Europe/Copenhagen")) + 1
from   <- with_tz(as_datetime(paste(target, "00:00"), tz = "Europe/Copenhagen"), "UTC")
to     <- from + days(1)

count_points <- function(area) {
  resp <- request("https://web-api.tp.entsoe.eu/api") |>
    req_url_query(securityToken = Sys.getenv("ENTSOE_TOKEN"), documentType = "A69", processType = "A01",
                  in_Domain = areas[[area]],
                  periodStart = format(from, "%Y%m%d%H%M"), periodEnd = format(to, "%Y%m%d%H%M")) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  txt <- resp_body_string(resp)
  reason <- if (resp_status(resp) == 200) "" else
    paste(regmatches(txt, gregexpr("(?<=<text>)[^<]*", txt, perl = TRUE))[[1]], collapse = " ")
  series <- regmatches(txt, gregexpr("(?s)<TimeSeries>.*?</TimeSeries>", txt, perl = TRUE))[[1]]
  n <- setNames(rep(0L, length(types)), types)
  for (s in series) {
    psr <- regmatches(s, regexpr("(?<=<psrType>)[^<]*", s, perl = TRUE))
    if (length(psr) && psr %in% names(types)) {
      n[[types[[psr]]]] <- n[[types[[psr]]]] + sum(gregexpr("<Point>", s, fixed = TRUE)[[1]] > 0)
    }
  }
  tibble(run_utc = format(now, "%Y-%m-%d %H:%M", tz = "UTC"), target = target, area = area,
         status = resp_status(resp), reason = reason, !!!as.list(n))
}

file <- "live/entsoe_timing.csv"
new  <- bind_rows(lapply(names(areas), count_points))
old  <- if (file.exists(file)) read_csv(file, show_col_types = FALSE, col_types = cols(.default = "c")) else NULL
write_csv(bind_rows(old, mutate(new, across(everything(), as.character))), file)
print(new)
