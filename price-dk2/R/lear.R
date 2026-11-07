
# lear functions


library(dplyr)
library(tidyr)
library(lubridate)
library(glmnet)

# days x 24 hours
build_daily <- function(prices) {
  daily <- prices |>
    mutate(local = with_tz(time, "Europe/Copenhagen"),
           date  = as_date(local),
           hour  = hour(local)) |>
    group_by(date, hour) |>
    summarise(price = mean(price), .groups = "drop") |>   # autumn dst: average double hour
    pivot_wider(names_from = hour, values_from = price, names_prefix = "h") |>
    complete(date = seq(min(date), max(date), by = "day")) |>
    select(date, all_of(paste0("h", 0:23))) |>
    arrange(date)
  
  # spring dst: fill hour 2
  daily |> mutate(h2 = if_else(is.na(h2), (h1 + h3) / 2, h2))
}

lag_days <- function(M, k) rbind(matrix(NA, k, ncol(M)), M[1:(nrow(M) - k), , drop = FALSE])

# lags D-1, D-2, D-3, D-7
make_features <- function(daily) {
  P <- as.matrix(daily[, -1])
  
  X_price <- cbind(lag_days(P, 1), lag_days(P, 2), lag_days(P, 3), lag_days(P, 7))
  colnames(X_price) <- paste0("d", rep(c(1, 2, 3, 7), each = 24), "_h", 0:23)
  
  dow   <- wday(daily$date, week_start = 1)   # 1 = monday
  X_dow <- sapply(1:7, \(d) as.numeric(dow == d))
  colnames(X_dow) <- paste0("dow", 1:7)
  
  list(P = P, X_price = X_price, X_dow = X_dow, dow = dow)
}

# asinh transform and inverse
transform_p <- function(x, med, s) asinh((x - med) / s)
inverse_p   <- function(z, med, s) med + s * sinh(z)

# lasso, lambda by bic
fit_lasso_bic <- function(X, y) {
  fit <- glmnet(X, y, alpha = 1)
  rss <- colSums((y - predict(fit, X))^2)
  n   <- length(y)
  bic <- n * log(rss / n) + fit$df * log(n)
  list(fit = fit, lambda = fit$lambda[which.min(bic)])
}

lear_forecast_day <- function(features, d, window = 730) {
  P     <- features$P
  train <- (d - window):(d - 1)
  
  med <- median(P[train, ], na.rm = TRUE)
  s   <- mad(P[train, ], na.rm = TRUE)
  
  X_train <- cbind(transform_p(features$X_price[train, ], med, s), features$X_dow[train, ])
  X_test  <- cbind(transform_p(features$X_price[d, , drop = FALSE], med, s),
                   features$X_dow[d, , drop = FALSE])
  if (anyNA(X_test)) stop("Missing input prices for the forecast day")
  
  sapply(1:24, \(h) {
    y    <- transform_p(P[train, h], med, s)
    keep <- complete.cases(X_train) & !is.na(y)
    m    <- fit_lasso_bic(X_train[keep, ], y[keep])
    as.numeric(inverse_p(predict(m$fit, X_test, s = m$lambda), med, s))
  })
}
