# spread for the intraday forecasts, with the spreads known at each issue time

library(dplyr)
library(tidyr)
library(lubridate)
library(parallel)
source("R/holidays.R")
source("R/features.R")
source("R/spread.R")

imb <- readRDS("data/raw/imbalance.rds")
q <- make_quarters(imb, transmute(imb, time, spot = SpotPriceEUR), readRDS("data/raw/weather.rds"),
                   dk_holidays(2025:2027)$date)
known <- q |> filter(!is.na(dir), !is.na(spread))
id <- readRDS("data/processed/intraday_forecasts.rds") |> filter(model %in% c("ens", "clim"))
cores <- as.integer(Sys.getenv("CORES", max(1, detectCores() - 1)))

out <- mclapply(split(id, id$issue), \(x) {
  until <- x$issue[1] - minutes(60)
  test  <- known[match(unique(x$time), known$time), ]
  test  <- test[!is.na(test$time), ]
  if (nrow(test) == 0) return(NULL)
  pools <- spread_pools(known, until)
  down  <- near_spot(pools$down, test$spot)
  up    <- near_spot(pools$up, test$spot)
  e     <- x[x$model == "ens", ]
  res <- list(clim    = clim_quantiles(known[known$time < until, ], test),
              mix_ens = mixture_quantiles(as.matrix(e[match(test$time, e$time), c("down", "none", "up")]), down, up),
              perfect = mixture_quantiles(outer(test$dir, c(down = -1, none = 0, up = 1), "==") * 1, down, up))
  b <- e$bucket[match(test$time, e$time)]
  bind_rows(lapply(names(res), \(m) tibble(issue = x$issue[1], time = test$time, bucket = b,
                                           date = test$date, spread = test$spread, model = m,
                                           as_tibble(res[[m]]))))
}, mc.cores = cores)
fc <- bind_rows(out)
saveRDS(fc, "data/processed/intraday_spread_forecasts.rds")

Q <- as.matrix(fc[, paste0("q", taus * 100)])
fc |>
  mutate(pinball = pinball(Q, spread), value = side_value(mean, spread)) |>
  group_by(bucket, model) |>
  summarise(pinball = mean(pinball), value = mean(value), .groups = "drop") |>
  print(n = Inf)
