# tests on daily series, newey-west with one week of lags (losses come in runs)

nw_var <- function(x, lag = 7) {
  n <- length(x)
  u <- x - mean(x)
  v <- sum(u^2) / n
  for (k in seq_len(min(lag, n - 1))) v <- v + 2 * (1 - k / (lag + 1)) * sum(u[-(1:k)] * u[1:(n - k)]) / n
  v
}

# diebold-mariano on daily loss differences, h = 1 (hln factor)
dm_test <- function(d, lag = 7) {
  n    <- length(d)
  stat <- mean(d) / sqrt(nw_var(d, lag) / n) * sqrt((n - 1) / n)
  c(stat = stat, p = 2 * pt(-abs(stat), df = n - 1))
}

# t test of a mean, like daily coverage vs its target
nw_test <- function(x, mu = 0, lag = 7) {
  stat <- (mean(x) - mu) / sqrt(nw_var(x, lag) / length(x))
  c(stat = stat, p = 2 * pnorm(-abs(stat)))
}
