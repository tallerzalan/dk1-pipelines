# lear backtest
# lago et al. (2021)


# one lasso per hour, daily
# lags D-1, D-2, D-3, D-7
# 2-year window, lambda by bic
# asinh((p - median) / mad)


library(ggplot2)
source("R/lear.R")   # same code as live

prices   <- readRDS("data/processed/prices_dk1_hourly.rds")
daily    <- build_daily(prices)
features <- make_features(daily)
P        <- features$P
dow      <- features$dow

# rolling forecasts
test_days <- which(daily$date >= as_date("2025-10-01") & complete.cases(P))
F_lear    <- matrix(NA, nrow(P), 24)

for (d in test_days) {
  fc <- tryCatch(lear_forecast_day(features, d, window = 730), error = \(e) NULL)
  if (!is.null(fc)) F_lear[d, ] <- fc

  if (which(test_days == d) %% 30 == 0) message("Forecasted up to ", daily$date[d])
}

# naive benchmark
F_naive <- lag_days(P, 1)
use_week <- dow %in% c(1, 6, 7)
F_naive[use_week, ] <- lag_days(P, 7)[use_week, ]

# comparison
results_long <- tibble(
  date      = rep(daily$date[test_days], each = 24),
  hour      = rep(0:23, times = length(test_days)),
  actual    = as.vector(t(P[test_days, ])),
  lear      = as.vector(t(F_lear[test_days, ])),
  naive_mix = as.vector(t(F_naive[test_days, ]))
) |>
  filter(!is.na(lear), !is.na(naive_mix))

results <- tibble(
  model = c("LEAR", "naive_mix"),
  MAE   = c(mean(abs(results_long$actual - results_long$lear)),
            mean(abs(results_long$actual - results_long$naive_mix))),
  RMSE  = c(sqrt(mean((results_long$actual - results_long$lear)^2)),
            sqrt(mean((results_long$actual - results_long$naive_mix)^2)))
)

print(results)
cat("LEAR improvement over naive (MAE):",
    round(100 * (1 - results$MAE[1] / results$MAE[2]), 1), "%\n")

# plot last 7 days
results_long |>
  filter(date > max(date) - 7) |>
  mutate(time = as_datetime(date, tz = "Europe/Copenhagen") + hours(hour)) |>
  ggplot(aes(time)) +
  geom_line(aes(y = actual,    colour = "Actual"),    linewidth = 0.6) +
  geom_line(aes(y = lear,      colour = "LEAR"),      linewidth = 0.5) +
  geom_line(aes(y = naive_mix, colour = "Naive mix"), linewidth = 0.4, linetype = "dashed") +
  scale_colour_manual(values = c("Actual" = "black", "LEAR" = "blue", "Naive mix" = "red")) +
  labs(title = "DK1 day-ahead price: LEAR vs naive, last 7 days",
       x = NULL, y = "EUR/MWh", colour = NULL) +
  theme_minimal()

# save lear forecasts
saveRDS(results_long, "data/processed/forecasts_lear.rds")
  
