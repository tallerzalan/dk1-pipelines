# direction backtest: all quarters of day D at 14:00 on D-1, refit weekly on the year before

library(dplyr)
library(tidyr)
library(lubridate)
library(parallel)
library(xgboost)
source("R/holidays.R")
source("R/features.R")
source("R/direction.R")

imb <- readRDS("data/raw/imbalance.rds")
q <- make_quarters(imb, transmute(imb, time, spot = SpotPriceEUR), readRDS("data/raw/weather.rds"),
                   dk_holidays(2025:2027)$date) |>
  add_recent()
ok <- complete.cases(q[, feature_cols]) & !is.na(q$dir)

window <- 365
cores  <- as.integer(Sys.getenv("CORES", max(1, detectCores() - 1)))
full_days <- q |> filter(ok) |> count(date) |> filter(n >= 92) |> pull(date)
refits    <- seq(as_date("2025-11-01"), max(full_days), by = "7 days")

t0 <- Sys.time()
out <- mclapply(refits, \(R) tryCatch({
  # fit on day R, used for forecasts made R to R+6
  train <- q[ok & q$date >= R - window & q$date < R, ]
  m_mnl <- fit_mnl(train)
  m_xgb <- fit_xgb(train, as.integer(R))
  days  <- full_days[full_days >= R + 1 & full_days <= R + 7]
  bind_rows(lapply(days, \(D) {
    test <- q[ok & q$date == D, ]
    known <- q[ok & q$date >= D - 30 & (q$date <= D - 2 | (q$date == D - 1 & q$hour < 12)), ]
    P <- list(clim = clim_probs(known, test), mnl = pred_mnl(m_mnl, test), xgb = pred_xgb(m_xgb, test))
    P$ens <- (P$mnl + P$xgb) / 2
    bind_rows(lapply(names(P), \(m) tibble(time = test$time, date = D, hour = test$hour, dir = test$dir,
                                           model = m, as_tibble(P[[m]]))))
  }))
}, error = \(e) NULL), mc.cores = cores)
cat("done in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")

fc <- bind_rows(out)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(fc, "data/processed/direction_forecasts.rds")

fc |>
  mutate(ll = log_loss(cbind(down, none, up), dir)) |>
  group_by(model) |>
  summarise(days = n_distinct(date), log_loss = mean(ll), .groups = "drop") |>
  print()
