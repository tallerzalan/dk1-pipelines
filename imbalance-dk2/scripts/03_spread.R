# spread backtest: day D at 14:00 on D-1, direction probabilities mixed with recent spreads
# benchmark = all spreads in that hour over 28 days, perfect = the true direction

library(dplyr)
library(tidyr)
library(lubridate)
source("R/holidays.R")
source("R/features.R")
source("R/spread.R")

imb <- readRDS("data/raw/imbalance.rds")
q <- make_quarters(imb, transmute(imb, time, spot = SpotPriceEUR), readRDS("data/raw/weather.rds"),
                   dk_holidays(2025:2027)$date)
known <- q |> filter(!is.na(dir), !is.na(spread))
dirfc <- readRDS("data/processed/direction_forecasts.rds")

out <- lapply(sort(unique(dirfc$date)), \(D) {
  until <- as_datetime(paste(D - 1, "12:00:00"), tz = "Europe/Copenhagen")
  test  <- known[known$date == D, ]
  if (nrow(test) == 0) return(NULL)
  pools <- spread_pools(known, until)
  down  <- near_spot(pools$down, test$spot)
  up    <- near_spot(pools$up, test$spot)

  res <- list(clim = clim_quantiles(known[known$time < until, ], test))
  for (m in setdiff(unique(dirfc$model), "clim")) {
    P <- dirfc |> filter(date == D, model == m)
    res[[paste0("mix_", m)]] <- mixture_quantiles(as.matrix(P[match(test$time, P$time), c("down", "none", "up")]),
                                                  down, up)
  }
  res$perfect <- mixture_quantiles(outer(test$dir, c(down = -1, none = 0, up = 1), "==") * 1, down, up)

  bind_rows(lapply(names(res), \(m) tibble(time = test$time, date = D, hour = test$hour,
                                           dir = test$dir, spread = test$spread, model = m,
                                           as_tibble(res[[m]]))))
})
fc <- bind_rows(out)
saveRDS(fc, "data/processed/spread_forecasts.rds")

Q <- as.matrix(fc[, paste0("q", taus * 100)])
fc |>
  mutate(pinball = pinball(Q, spread), value = side_value(mean, spread)) |>
  group_by(model) |>
  summarise(pinball = mean(pinball), value = mean(value), .groups = "drop") |>
  print()
