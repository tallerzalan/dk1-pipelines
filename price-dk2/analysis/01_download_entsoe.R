# HINDSIGHT ONLY - data published after the auction. Never a forecast input.

# actual generation by type and actual load for dk1 and germany
# from entso-e, hourly in danish time. needs ENTSOE_TOKEN in .Renviron. writes analysis/data/

library(httr2)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

areas <- c(DK1 = "10YDK-1--------W", DE = "10Y1001A1001A82H")
psr <- c(B01 = "biomass", B02 = "lignite", B04 = "gas", B05 = "hard_coal", B06 = "oil", B10 = "hydro_pumped",
         B11 = "hydro_river", B12 = "hydro_reservoir", B14 = "nuclear", B15 = "other_renewable", B16 = "solar",
         B17 = "waste", B18 = "wind_offshore", B19 = "wind_onshore", B20 = "other")
from <- as_date("2025-10-01")
to   <- today()

tag  <- function(x, name) regmatches(x, regexpr(paste0("(?<=<", name, ">)[^<]*"), x, perl = TRUE))
tags <- function(x, name) regmatches(x, gregexpr(paste0("(?<=<", name, ">)[^<]*"), x, perl = TRUE))[[1]]

get_xml <- function(params) {
  Sys.sleep(1)
  resp <- request("https://web-api.tp.entsoe.eu/api") |>
    req_url_query(securityToken = Sys.getenv("ENTSOE_TOKEN"), !!!params) |>
    req_retry(max_tries = 4, backoff = \(i) 10) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  txt <- resp_body_string(resp)
  if (grepl("<Acknowledgement_MarketDocument", txt, fixed = TRUE)) {
    message("ENTSO-E: ", paste(tag(txt, "text"), collapse = " "))
    return(NULL)
  }
  txt
}

# one row per point, with the series' production type
points <- function(txt) {
  series <- regmatches(txt, gregexpr("(?s)<TimeSeries>.*?</TimeSeries>", txt, perl = TRUE))[[1]]
  bind_rows(lapply(series, \(s) {
    if (grepl("outBiddingZone_Domain", s, fixed = TRUE) && grepl("psrType", s, fixed = TRUE)) return(NULL)  # pumping, not generation
    type <- tag(s, "psrType")
    periods <- regmatches(s, gregexpr("(?s)<Period>.*?</Period>", s, perl = TRUE))[[1]]
    bind_rows(lapply(periods, \(p) {
      t0   <- as.POSIXct(tag(p, "start"), format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
      t1   <- as.POSIXct(tag(p, "end"), format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
      step <- c(PT15M = 15, PT30M = 30, PT60M = 60)[[tag(p, "resolution")]]
      n    <- as.integer(difftime(t1, t0, units = "mins")) %/% step
      v    <- rep(NA_real_, n)
      v[as.integer(tags(p, "position"))] <- as.numeric(tags(p, "quantity"))
      tibble(type = if (length(type)) type else "load", time = t0 + (seq_len(n) - 1) * step * 60, mw = v)
    }))
  }))
}

# monthly requests, generation (A75) and load (A65), actuals (A16)
months <- seq(floor_date(from, "month"), to, by = "month")
raw <- bind_rows(lapply(names(areas), \(a) bind_rows(lapply(months, \(m) {
  p <- list(periodStart = format(as_datetime(m), "%Y%m%d%H%M"),
            periodEnd   = format(as_datetime(min(m %m+% months(1), to)), "%Y%m%d%H%M"))
  message(a, " ", m)
  gen  <- get_xml(c(list(documentType = "A75", processType = "A16", in_Domain = areas[[a]]), p))
  load <- get_xml(c(list(documentType = "A65", processType = "A16", outBiddingZone_Domain = areas[[a]]), p))
  bind_rows(if (!is.null(gen)) points(gen), if (!is.null(load)) points(load)) |> mutate(area = a)
}))))

# hourly averages in danish time (the double hour in october is averaged, as for prices)
hourly <- raw |>
  mutate(type = coalesce(psr[type], type),
         local = with_tz(time, "Europe/Copenhagen"), date = as_date(local), hour = hour(local)) |>
  group_by(area, type, date, hour) |>
  summarise(mw = mean(mw, na.rm = TRUE), .groups = "drop") |>
  filter(is.finite(mw))

dir.create("analysis/data", recursive = TRUE, showWarnings = FALSE)
write_csv(hourly, "analysis/data/entsoe_actual.csv")
hourly |> count(area, type) |> print(n = Inf)
