# open-meteo sites and download
# previous_day2 = made 48h before

library(httr2)
library(dplyr)
library(tidyr)
library(lubridate)

sites <- tribble(
  ~site,           ~lat,  ~lon,  ~type,
  "horns_rev",     55.53, 7.91,  "wind",    # offshore, north sea
  "west_jutland",  56.10, 8.30,  "wind",    # onshore, west coast
  "north_jutland", 57.05, 9.30,  "wind",
  "south_jutland", 55.20, 9.20,  "solar",
  "mid_jutland",   56.20, 9.50,  "solar",
  "funen",         55.30, 10.30, "solar"
)
# germany: offshore north sea and baltic, the windy north, solar in the south and east
sites_de <- tribble(
  ~site,               ~lat,  ~lon,  ~type,
  "de_north_sea",      54.20, 6.50,  "wind",
  "de_baltic",         54.80, 14.00, "wind",
  "de_schleswig",      54.40, 9.20,  "wind",
  "de_lower_saxony",   52.90, 8.00,  "wind",
  "de_brandenburg",    53.00, 13.00, "wind",
  "de_bavaria",        48.60, 11.50, "solar",
  "de_baden",          48.70, 9.20,  "solar",
  "de_saxony",         51.10, 13.00, "solar"
)
api_var <- c(wind = "wind_speed_100m_previous_day2", solar = "shortwave_radiation_previous_day2")

get_site <- function(site, lat, lon, type, from, to) {
  var <- api_var[[type]]
  resp <- request("https://previous-runs-api.open-meteo.com/v1/forecast") |>
    req_url_query(latitude = lat, longitude = lon, hourly = var,
                  start_date = format(from), end_date = format(to), timezone = "GMT") |>
    req_retry(max_tries = 6, retry_on_failure = TRUE, backoff = \(i) min(15 * 2^i, 240)) |>  # 30s, 1, 2, 4, 4 min
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  tibble(time  = ymd_hm(resp$hourly$time, tz = "UTC"),
         name  = paste0(type, "_", site),
         value = as.numeric(resp$hourly[[var]]))
}

# all sites, hourly utc
get_weather <- function(from, to, s = sites) {
  periods <- tibble(from = seq(as_date(from), as_date(to), by = "quarter")) |>
    mutate(to = pmin(lead(from, default = as_date(to) + 1) - 1, as_date(to)))
  jobs <- crossing(s, periods)
  lapply(seq_len(nrow(jobs)), \(i) {
    j <- jobs[i, ]
    message(j$site, " ", j$from)
    Sys.sleep(0.5)
    get_site(j$site, j$lat, j$lon, j$type, j$from, j$to)
  }) |>
    bind_rows() |>
    pivot_wider(names_from = name, values_from = value) |>
    arrange(time)
}
