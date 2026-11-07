# battery backtest, runs daily
# plan on forecast, paid actual
# perfect = actual prices

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)

source("R/lear.R")
source("R/battery.R")
source("R/forecasts.R")

# see R/battery.R
cap_mwh   <- battery_setup$cap_mwh
power_mw  <- battery_setup$power_mw
eff_rt    <- battery_setup$eff_rt
max_cycle <- battery_setup$max_cycle

# same forecasts as 10
models <- c("ensemble", "bmtx", "learx", "xgbx", "naive_mix")
fc <- all_forecasts(models)


lp_def <- battery_lp()

# plan with each forecast
strategies <- c(perfect = "actual", setNames(models, models))
days <- unique(fc$date)

profit <- lapply(days, \(dd) {
  x <- fc[fc$date == dd, ]
  sapply(strategies, \(col) battery_profit(x[[col]], x$actual, lp_def))
})
profit <- bind_cols(tibble(date = days), as_tibble(do.call(rbind, profit)))


# summary
years   <- nrow(profit) / 365
perfect <- sum(profit$perfect)

# mae on same days
mae <- sapply(strategies, \(col) mean(abs(fc$actual - fc[[col]])))

summary_tbl <- profit |>
  pivot_longer(-date, names_to = "strategy", values_to = "profit") |>
  group_by(strategy) |>
  summarise(total_eur        = round(sum(profit)),
            eur_per_mw_year  = round(sum(profit) / power_mw / years),
            share_of_perfect = round(sum(profit) / perfect, 3)) |>
  mutate(mae = round(mae[strategy], 2)) |>
  arrange(desc(total_eur))

print(summary_tbl)


# profit tests, newey-west
nw_test <- function(d, lag = 7) {
  n <- length(d)
  e <- d - mean(d)
  v <- sum(e^2) / n
  for (k in 1:lag) v <- v + 2 * (1 - k / (lag + 1)) * sum(e[-(1:k)] * e[1:(n - k)]) / n
  t <- mean(d) / sqrt(v / n)
  c(mean_eur_per_day = mean(d), t = t, p_value = 2 * pnorm(-abs(t)))
}

comparisons <- combn(models, 2, simplify = FALSE)
tests <- bind_rows(lapply(comparisons, \(ab) {
  r <- nw_test(profit[[ab[1]]] - profit[[ab[2]]])
  tibble(model_a = ab[1], model_b = ab[2], mean_eur_per_day = r[["mean_eur_per_day"]],
         t = r[["t"]], p_value = r[["p_value"]])
}))
print(tests)


# cumulative profit
p <- profit |>
  pivot_longer(-date, names_to = "strategy", values_to = "profit") |>
  group_by(strategy) |>
  mutate(cum_profit = cumsum(profit)) |>
  ggplot(aes(date, cum_profit, colour = strategy)) +
  geom_line(linewidth = 0.7) +
  labs(title = "Battery profit when planning on each forecast",
       subtitle = paste0(cap_mwh, " MWh / ", power_mw, " MW battery, ",
                         eff_rt * 100, "% round trip, max ", max_cycle, " cycle per day"),
       x = NULL, y = "Cumulative profit (EUR)", colour = NULL) +
  theme_minimal()
print(p)

# for the dashboard
dir.create("results", showWarnings = FALSE)
write_csv(mutate(profit, across(-date, \(x) round(x, 2))) |>
            left_join(distinct(fc, date, source), by = "date"),
          "results/battery_daily.csv")
write_csv(summary_tbl, "results/battery_summary.csv")
write_csv(tests, "results/battery_tests.csv")
write_csv(tibble(name  = c("cap_mwh", "power_mw", "eff_rt", "max_cycle"),
                 value = c(cap_mwh, power_mw, eff_rt, max_cycle)),
          "results/battery_setup.csv")
