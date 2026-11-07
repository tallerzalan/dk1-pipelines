# intraday backtest: every hour, the next 4 hours, one model per hour ahead, refit weekly

library(dplyr)
library(tidyr)
library(lubridate)
library(parallel)
library(xgboost)
source("R/holidays.R")
source("R/features.R")
source("R/direction.R")
source("R/intraday.R")

imb <- readRDS("data/raw/imbalance.rds")
q <- make_quarters(imb, transmute(imb, time, spot = SpotPriceEUR), readRDS("data/raw/weather.rds"),
                   dk_holidays(2025:2027)$date)
issues <- seq(as_datetime("2025-03-12 00:00:00"), max(q$time) - hours(4), by = "hour")
base <- c("hour", "qh", "wday", "holiday", "new_rule", "spot", "spot_rel", "spot_rank", "spot_diff",
          "spot_neg", "wpow", "solar")
# real-time inputs only help in the first hour, so only used there
cols_for <- function(b) c(intraday_cols, if (b == 1) realtime_cols)
d <- make_issues(q, make_state(q, imb), issues) |>
  left_join(make_realtime(readRDS("data/raw/realtime.rds"), readRDS("data/raw/energinet_forecasts.rds"),
                          issues), by = "issue")
d <- d[complete.cases(d[, c(base, intraday_cols)]) & !is.na(d$dir), ]
d <- d[d$bucket > 1 | complete.cases(d[, realtime_cols]), ] |>
  mutate(date = as_date(with_tz(time, "Europe/Copenhagen")))

window <- 365
cores  <- as.integer(Sys.getenv("CORES", max(1, detectCores() - 1)))
refits <- seq(as_datetime("2025-11-01 00:00:00", tz = "Europe/Copenhagen"), max(d$issue), by = "7 days")
jobs   <- crossing(refit = refits, b = 1:4)

t0 <- Sys.time()
out <- mclapply(seq_len(nrow(jobs)), \(j) tryCatch({
  R <- jobs$refit[j]; b <- jobs$b[j]; cols <- cols_for(b)
  train <- d |> filter(bucket == b, time < R - minutes(60), time >= R - days(window))
  test  <- d |> filter(bucket == b, issue >= R, issue < R + days(7))
  if (nrow(test) == 0) return(NULL)
  P <- list(clim = clim_probs(train, test),
            mnl  = mnl_probs(train, test, extra = cols),
            xgb  = xgb_probs(train, test, as.integer(as_date(R)), cols = c(base, cols, "lead")))
  P$ens <- (P$mnl + P$xgb) / 2
  bind_rows(lapply(names(P), \(m) tibble(issue = test$issue, time = test$time, bucket = b,
                                         lead = test$lead, dir = test$dir, model = m,
                                         as_tibble(P[[m]]))))
}, error = \(e) NULL), mc.cores = cores)
cat("done in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")

fc <- bind_rows(out)
saveRDS(fc, "data/processed/intraday_forecasts.rds")
fc |>
  mutate(ll = log_loss(cbind(down, none, up), dir)) |>
  group_by(bucket, model) |>
  summarise(log_loss = mean(ll), .groups = "drop") |>
  pivot_wider(names_from = model, values_from = log_loss) |>
  print()
