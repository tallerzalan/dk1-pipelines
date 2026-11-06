# bmt backtest, prices only
# same setup as 04_lear.R

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
source("R/lear.R")
source("R/bmt.R")

prices   <- readRDS("data/processed/prices_dk1_hourly.rds")
daily    <- build_daily(prices)
features <- make_features(daily)
P        <- features$P

pval   <- 0.05
delta1 <- 1
delta2 <- 1

test_days <- which(daily$date >= as_date("2025-10-01") & complete.cases(P))
F_bmt     <- matrix(NA, nrow(P), 24)
n_sel     <- matrix(NA, nrow(P), 24)
sel_count <- 0

for (d in test_days) {
  out <- tryCatch(bmt_forecast_day(features, d, pval = pval, delta1 = delta1, delta2 = delta2,
                                   return_selected = TRUE),
                  error = \(e) NULL)
  if (is.null(out)) next
  F_bmt[d, ] <- out$forecast
  n_sel[d, ] <- rowSums(out$selected)
  sel_count  <- sel_count + out$selected

  if (which(test_days == d) %% 30 == 0) message("Forecasted up to ", daily$date[d])
}

results_long <- tibble(
  date   = rep(daily$date[test_days], each = 24),
  hour   = rep(0:23, times = length(test_days)),
  actual = as.vector(t(P[test_days, ])),
  bmt    = as.vector(t(F_bmt[test_days, ]))
) |>
  filter(!is.na(bmt))

# compare with lear
lear  <- readRDS("data/processed/forecasts_lear.rds") |> select(date, hour, lear, naive_mix)
naive <- tibble(date      = rep(daily$date + 1, each = 24),
                hour      = rep(0:23, times = nrow(P)),
                naive_day = as.vector(t(P)))
comp <- results_long |>
  inner_join(lear, by = c("date", "hour")) |>
  inner_join(naive, by = c("date", "hour"))

results <- tibble(
  model = c("BMT", "LEAR", "naive_day", "naive_mix"),
  MAE   = sapply(c("bmt", "lear", "naive_day", "naive_mix"), \(m) mean(abs(comp$actual - comp[[m]]))),
  RMSE  = sapply(c("bmt", "lear", "naive_day", "naive_mix"), \(m) sqrt(mean((comp$actual - comp[[m]])^2)))
)
print(results)
cat("hours compared:", nrow(comp), "\n")
cat("average number of selected prices per hour:", round(mean(n_sel[test_days, ], na.rm = TRUE), 1), "\n")

# selection share per hour
sel_share <- sel_count / length(test_days)

# heatmap
as_tibble(sel_share, rownames = "target") |>
  pivot_longer(-target, names_to = "input", values_to = "share") |>
  mutate(target = factor(target, levels = paste0("h", 0:23)),
         input  = factor(input, levels = colnames(features$X_price))) |>
  ggplot(aes(input, target, fill = share)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#f97316") +
  labs(title = "BMT: how often each lagged price is selected",
       x = "input (day lag and hour)", y = "forecast hour", fill = "share") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 5))

saveRDS(results_long, "data/processed/forecasts_bmt.rds")
saveRDS(sel_share, "data/processed/bmt_selection.rds")

# selection share, for dashboard
dir.create("results", showWarnings = FALSE)
readr::write_csv(as_tibble(sel_share, rownames = "target") |>
                   pivot_longer(-target, names_to = "input", values_to = "share") |>
                   mutate(share = round(share, 4)),
                 "results/bmt_selection.csv")
