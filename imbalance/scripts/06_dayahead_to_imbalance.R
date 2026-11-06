# day-ahead to imbalance, backtest. a position on day-ahead without physical power is settled at the
# imbalance price, so buying (selling) on day-ahead is a bet that the imbalance price ends above
# (below) the spot price
#
# set before running (14 oct 2026):
# - decision at 10:45 on D-1, before the noon auction, for every quarter of D
# - known then: imbalances of D-1 up to 09:00, the price repo's forecast for D (the spot price isn't
#   known before the auction), weather made 48 hours ahead, the calendar
# - direction: the day-ahead models of 02_direction.R (multinomial lasso, xgboost, their average),
#   refit weekly on the year before. trained on actual spot prices, fed the price forecast for D.
#   benchmark: the 28-day share per hour
# - spread: the mixture of 03_spread.R, past spreads matched on the price forecast
# - position: long 1 MW if the expected spread is above the fees (0.188 €/MWh), short if below minus
#   the fees, otherwise none. P&L = position x (imbalance - spot) - fees, per quarter
# - compared on daily P&L with always long, always short and the benchmark forecast with the same rule
# - tests with newey-west (7 lags). worth trading in the backtest only if the mean daily P&L is above 0
#   with p < 0.05 and it beats all three with p < 0.05 (holm). it runs live as paper trading either way

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(parallel)
library(xgboost)
source("R/holidays.R")
source("R/features.R")
source("R/direction.R")
source("R/spread.R")
source("R/da_imbalance.R")

imb     <- readRDS("data/raw/imbalance.rds")
weather <- readRDS("data/raw/weather.rds")
hol     <- dk_holidays(2025:2027)$date
pf      <- price_forecasts()

# history with actual spot prices: training rows and past spreads
q     <- make_quarters(imb, transmute(imb, time, spot = SpotPriceEUR), weather, hol) |>
  add_recent(morning_until = da_cutoff_hour)
ok    <- complete.cases(q[, feature_cols]) & !is.na(q$dir)
known <- q |> filter(!is.na(dir), !is.na(spread))

window    <- 365
cores     <- as.integer(Sys.getenv("CORES", max(1, detectCores() - 1)))
full_days <- q |> filter(ok) |> count(date) |> filter(n >= 92) |> pull(date)
test_days <- full_days[full_days %in% pf$date & full_days >= as_date("2025-11-02")]
refits    <- seq(as_date("2025-11-01"), max(test_days), by = "7 days")

t0 <- Sys.time()
out <- mclapply(refits, \(R) tryCatch({
  # fit on day R, used for the decisions on R to R+6 (days R+1 to R+7)
  train <- q[ok & q$date >= R - window & q$date < R, ]
  m_mnl <- fit_mnl(train)
  m_xgb <- fit_xgb(train, as.integer(R))
  days  <- test_days[test_days >= R + 1 & test_days <= R + 7]
  bind_rows(lapply(days, \(D) {
    test <- da_rows(D, imb, pf, weather, hol)
    test <- test[complete.cases(test[, feature_cols]) & !is.na(test$spread), ]
    if (nrow(test) != length(quarter_times(D))) return(NULL)   # whole days only
    until <- da_until(D)
    P <- list(clim = clim_probs(known[known$date >= D - 30 & known$time < until, ], test),
              mnl  = pred_mnl(m_mnl, test), xgb = pred_xgb(m_xgb, test))
    P$ens <- (P$mnl + P$xgb) / 2
    pools   <- spread_pools(known, until)
    near_dn <- near_spot(pools$down, test$spot)
    near_up <- near_spot(pools$up, test$spot)
    bind_rows(lapply(names(P), \(m) {
      expected <- mixture_quantiles(P[[m]], near_dn, near_up)[, "mean"]
      tibble(time = test$time, date = D, hour = test$hour, spot_fc = test$spot, dir = test$dir,
             spread = test$spread, model = m, as_tibble(P[[m]]), expected = expected)
    }))
  }))
}, error = \(e) { message("refit ", R, ": ", conditionMessage(e)); NULL }), mc.cores = cores)
cat("done in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")

fc <- bind_rows(out)
saveRDS(fc, "data/processed/da_imbalance_forecasts.rds")

# P&L per quarter for 1 MW, then per day
quarters <- fc |>
  filter(model %in% c("ens", "clim")) |>
  select(time, date, spread, model, expected) |>
  pivot_wider(names_from = model, values_from = expected, names_prefix = "exp_")
pnl <- quarters |>
  transmute(time, date,
            ens     = da_pnl(da_position(exp_ens), spread),
            clim    = da_pnl(da_position(exp_clim), spread),
            long    = da_pnl(1, spread),
            short   = da_pnl(-1, spread),
            perfect = da_pnl(sign(spread), spread))
daily <- pnl |> group_by(date) |> summarise(across(-time, sum), .groups = "drop")

strategies <- c(ens = "Forecast", clim = "Benchmark forecast", long = "Always long", short = "Always short",
                perfect = "Perfect foresight")
share_traded <- c(ens = mean(da_position(quarters$exp_ens) != 0), clim = mean(da_position(quarters$exp_clim) != 0),
                  long = 1, short = 1, perfect = mean(quarters$spread != 0))
summary <- bind_rows(lapply(names(strategies), \(s) {
  x  <- daily[[s]]
  r  <- nw_test(x)
  sh <- share_traded[[s]]
  tibble(strategy = s, label = strategies[[s]], days = length(x), mean_day = mean(x), total = sum(x),
         p = if (s == "perfect") NA_real_ else r[["p"]], days_positive = mean(x > 0),
         worst_day = min(x), best_day = max(x), traded = sh,
         per_mwh = sum(x) / (sh * nrow(quarters) * 0.25))
}))

# the forecast against each alternative, holm over the three
tests <- bind_rows(lapply(c("clim", "long", "short"), \(s) {
  r <- nw_test(daily$ens - daily[[s]])
  tibble(versus = s, label = strategies[[s]], diff_day = r[["mean"]], p = r[["p"]])
})) |>
  mutate(p_holm = p.adjust(p, "holm"))

ll <- fc |>
  filter(model %in% c("ens", "clim"), !is.na(dir)) |>
  mutate(ll = log_loss(cbind(down, none, up), dir)) |>
  group_by(model) |>
  summarise(ll = mean(ll), .groups = "drop")
ens <- summary[summary$strategy == "ens", ]
worth <- ens$mean_day > 0 && ens$p < 0.05 && all(tests$diff_day > 0 & tests$p_holm < 0.05)

print(summary, width = Inf)
print(tests)
cat("direction skill before the auction:", round(1 - ll$ll[ll$model == "ens"] / ll$ll[ll$model == "clim"], 4),
    "\nworth trading in the backtest:", worth, "\n")

dir.create("results", showWarnings = FALSE)
write_csv(mutate(daily, across(-date, \(x) round(x, 2))), "results/da_imbalance_daily.csv")
write_csv(summary, "results/da_imbalance_summary.csv")
write_csv(tests, "results/da_imbalance_tests.csv")
write_csv(tibble(name  = c("from", "to", "days", "fee", "cutoff_hour", "direction_skill", "worth_trading"),
                 value = c(format(min(daily$date)), format(max(daily$date)), nrow(daily), da_fee,
                           da_cutoff_hour, round(1 - ll$ll[ll$model == "ens"] / ll$ll[ll$model == "clim"], 4),
                           worth)),
          "results/da_imbalance_info.csv")
