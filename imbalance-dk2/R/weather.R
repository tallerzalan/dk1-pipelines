# open-meteo sites and download
# previous_day2 = made 48h before, so before the auction

library(httr2)
library(dplyr)
library(tidyr)
library(lubridate)

sites <- tribble(
  ~site,           ~lat,  ~lon,  ~type,
  "kriegers_flak", 55.05, 12.90, "wind",    # offshore, baltic
  "rodsand",       54.55, 11.65, "wind",    # offshore, south of lolland
  "lolland",       54.80, 11.30, "wind",    # onshore
  "lolland_falster", 54.80, 11.90, "solar",
  "mid_zealand",   55.45, 11.80, "solar",
  "north_zealand", 55.85, 12.30, "solar"
)
api_var <- c(wind = "wind_speed_100m_previous_day2", solar = "shortwave_radiation_previous_day2")

get_site <- function(site, lat, lon, type, from, to) {
  var <- api_var[[type]]
  resp <- request("https://previous-runs-api.open-meteo.com/v1/forecast") |>
    req_url_query(latitude = lat, longitude = lon, hourly = var,
                  start_date = format(from), end_date = format(to), timezone = "GMT") |>
    req_retry(max_tries = 4, retry_on_failure = TRUE, backoff = \(i) 15 * 2^i) |>  # 30s, 1, 2 min, then the saved weather
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  tibble(time  = ymd_hm(resp$hourly$time, tz = "UTC"),
         name  = paste0(type, "_", site),
         value = as.numeric(resp$hourly[[var]]))
}

# all sites, one row per hour (utc), a quarter at a time
get_weather <- function(from, to) {
  periods <- tibble(from = seq(as_date(from), as_date(to), by = "quarter")) |>
    mutate(to = pmin(lead(from, default = as_date(to) + 1) - 1, as_date(to)))
  jobs <- crossing(sites, periods)
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
