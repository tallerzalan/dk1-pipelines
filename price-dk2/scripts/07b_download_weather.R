# open-meteo, made 48h ahead

source("R/weather.R")   # sites and get_weather()

weather <- get_weather(as_date("2024-01-01"), Sys.Date() + 1)

# coverage
print(bind_rows(lapply(setdiff(names(weather), "time"), \(v) {
  x <- weather[[v]]
  first <- which(!is.na(x))[1]
  tibble(variable = v, from = weather$time[first],
         missing_pct = round(100 * mean(is.na(x[first:length(x)])), 2))
})))

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
saveRDS(weather, "data/raw/weather_openmeteo.rds")
cat("saved", nrow(weather), "hours from", format(min(weather$time)), "to", format(max(weather$time)), "\n")
