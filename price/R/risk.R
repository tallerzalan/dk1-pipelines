# revenue at risk for the battery: the plan is fixed, so revenue = planned + sum(error * plan)

alpha_r   <- 0.05    # 95% var and es
k_r       <- 60      # days of 24-hour errors, as for the ranges
k_r_long  <- 250     # days for the ewma method
lambda_r  <- 0.94    # ewma weight, riskmetrics' daily value
r_methods <- c(hs = "Historical errors", fhs = "Scaled by price volatility", normal = "Normal",
               ewma = "Scaled by recent errors (EWMA)")

# expected error volatility per day: ewma of the daily mean squared error, from the first 30 days
ewma_vol <- function(E, lambda = lambda_r) {
  m <- rowMeans(E^2)
  v <- numeric(length(m))
  v[1] <- mean(head(m, 30), na.rm = TRUE)
  for (i in seq_along(m)[-1]) v[i] <- if (is.na(m[i - 1])) v[i - 1] else lambda * v[i - 1] + (1 - lambda) * m[i - 1]
  sqrt(v)
}

# expected revenue, floor (alpha quantile) and mean below it
tail_stats <- function(r, alpha = alpha_r) {
  v <- quantile(r, alpha, names = FALSE)
  c(expected = mean(r), floor = v, tail = mean(r[r <= v]))
}

tail_stats_normal <- function(mu, s, alpha = alpha_r) {
  z <- qnorm(1 - alpha)
  c(expected = mu, floor = mu - z * s, tail = mu - s * dnorm(z) / alpha)
}

xlogy <- function(x, y) if (x == 0) 0 else x * log(y)

# kupiec: share of misses vs alpha, exact binomial p
kupiec <- function(miss, alpha = alpha_r) {
  n  <- length(miss)
  lr <- function(x) -2 * (xlogy(n - x, 1 - alpha) + xlogy(x, alpha) - xlogy(n - x, 1 - x / n) - xlogy(x, x / n))
  all_lr <- sapply(0:n, lr)
  sum(dbinom(0:n, n, alpha)[all_lr >= lr(sum(miss)) - 1e-9])
}

# christoffersen: do misses cluster, p simulated
christoffersen <- function(miss, sims = 10000) {
  lr <- function(m) {
    a <- head(m, -1); b <- m[-1]
    n00 <- sum(!a & !b); n01 <- sum(!a & b); n10 <- sum(a & !b); n11 <- sum(a & b)
    p   <- (n01 + n11) / length(b)
    p01 <- n01 / max(n00 + n01, 1); p11 <- n11 / max(n10 + n11, 1)
    -2 * (xlogy(n00 + n10, 1 - p) + xlogy(n01 + n11, p) -
            xlogy(n00, 1 - p01) - xlogy(n01, p01) - xlogy(n10, 1 - p11) - xlogy(n11, p11))
  }
  if (sum(miss) == 0) return(NA_real_)
  obs <- lr(miss)
  sim <- replicate(sims, lr(runif(length(miss)) < mean(miss)))
  (1 + sum(sim >= obs - 1e-9)) / (1 + sims)
}

# acerbi-szekely z2 for the es, > 0 means bad days are worse than the es says, p simulated
z2_stat <- function(x, var, es, alpha = alpha_r) sum((x / es)[x > var]) / (length(x) * alpha) - 1

z2_test <- function(x, var, es, draw, sims = 5000, alpha = alpha_r) {
  obs <- z2_stat(x, var, es, alpha)
  sim <- replicate(sims, z2_stat(draw(), var, es, alpha))
  c(z2 = obs, p = (1 + sum(sim >= obs)) / (1 + sims))
}

# fz0 loss (patton, ziegel & chen 2019), scores var and es together, needs e < 0
fz0 <- function(y, v, e, alpha = alpha_r) -(y <= v) * (v - y) / (alpha * e) + v / e + log(-e) - 1
