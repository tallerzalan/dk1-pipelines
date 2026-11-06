# naive benchmarks


# forecast D on D-1

library(dplyr)
library(lubridate)
library(ggplot2)

prices <- readRDS("data/processed/prices_dk1_hourly.rds")

# lags by danish date and hour (dst)
prices <- prices |>
  mutate(
    local   = with_tz(time, "Europe/Copenhagen"),
    key     = paste(as_date(local), hour(local)),
    lag_24  = price[match(paste(as_date(local) - 1, hour(local)), key)],   # same hour yesterday
    lag_168 = price[match(paste(as_date(local) - 7, hour(local)), key)],   # same hour last week
    weekday = wday(local, week_start = 1)  # 1 = monday
  )

# benchmark forecast
forecasts <- prices |>
  mutate(
    naive_day  = lag_24,
    naive_week = lag_168,
    # naive mix: yesterday tue-fri
    # last week mon, sat, sun
    naive_mix  = if_else(weekday %in% c(1, 6, 7), lag_168, lag_24)
  ) |>
  select(time, price, naive_day, naive_week, naive_mix)

# test period from oct 2025
test <- forecasts |>
  filter(time >= as_datetime("2025-10-01", tz = "UTC")) |>
  filter(!is.na(price))

# mae and rmse
results <- tibble(
  model = c("naive_day", "naive_week", "naive_mix"),
  MAE   = sapply(model, \(m) mean(abs(test$price - test[[m]]), na.rm = TRUE)),
  RMSE  = sapply(model, \(m) sqrt(mean((test$price - test[[m]])^2, na.rm = TRUE)))
) |>
  arrange(MAE)

print(results)

# example week
week_start <- max(test$time) - days(7)

test |>
  filter(time >= week_start) |>
  ggplot(aes(time)) +
  geom_line(aes(y = price,     colour = "Actual"),    linewidth = 0.6) +
  geom_line(aes(y = naive_mix, colour = "Naive mix"), linewidth = 0.4, linetype = "dashed") +
  geom_line(aes(y = naive_day, colour = "Naive day"), linewidth = 0.4, linetype = "dashed") +
  geom_line(aes(y = naive_week, colour = "Naive week"), linewidth = 0.4, linetype = "dashed") +
  scale_colour_manual(values = c("Actual" = "black", "Naive mix" = "red", "Naive day" = "blue", "Naive week" = "darkgreen")) +
  labs(title = "DK1 price vs naive forecast, last 7 days",
       x = NULL, y = "EUR/MWh", colour = NULL) +
  theme_minimal()

# save
saveRDS(forecasts, "data/processed/forecasts_benchmarks.rds")

