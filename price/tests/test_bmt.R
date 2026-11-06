# bmt_select() vs boosting_glm()

source("R/boosting_glm.R")
source("R/bmt.R")

check <- function(y, X, Xcond, pval = 0.05, delta1 = 1, delta2 = 1) {
  slow <- boosting_glm(y, X, Xcond, pval = pval, delta1 = delta1, delta2 = delta2, link = "mean")
  fast <- bmt_select(y, X, Xcond, pval = pval, delta1 = delta1, delta2 = delta2)
  identical(slow, fast)
}

# simulated data
set.seed(1)
ok <- sapply(1:40, \(i) {
  Tn <- 300; N <- 40
  X  <- matrix(rnorm(Tn * N), Tn, N) + rnorm(Tn) * 0.5
  if (i > 20) { X[, 3] <- 1; X[, 4] <- 0 }   # constant columns
  Xc <- matrix(rnorm(Tn * 2), Tn, 2)
  b  <- if (i %% 2 == 0) c(1, -0.7, 0.4) else c(0.15, 0, 0)   # some with weak signal
  y  <- 1 + X[, 1] * b[1] + X[, 5] * b[2] + X[, 12] * b[3] + Xc %*% c(0.3, -0.2) + rnorm(Tn)
  check(drop(y), X, Xc, pval = sample(c(0.01, 0.05, 0.1), 1),
        delta1 = sample(1:2, 1), delta2 = sample(1:2, 1))
})
cat("simulated:", sum(ok), "of", length(ok), "identical\n")

# real prices, one hour
if (file.exists("data/processed/prices_dk1_hourly.rds")) {
  source("R/lear.R")
  prices   <- readRDS("data/processed/prices_dk1_hourly.rds")
  daily    <- build_daily(prices)
  features <- make_features(daily)
  d     <- nrow(daily) - 30
}
if (exists("d") && d > 737) {
  train <- (d - 730):(d - 1)
  P     <- features$P
  med   <- median(P[train, ], na.rm = TRUE)
  s     <- mad(P[train, ], na.rm = TRUE)
  X     <- transform_p(features$X_price[train, ], med, s)
  D     <- features$X_dow[train, -1]
  real_ok <- sapply(c(1, 8, 13, 19), \(h) {
    y    <- transform_p(P[train, h], med, s)
    keep <- complete.cases(X) & !is.na(y)
    check(y[keep], X[keep, ], D[keep, ])
  })
  cat("real prices:", sum(real_ok), "of", length(real_ok), "identical\n")
  ok <- c(ok, real_ok)
}

stopifnot(all(ok))

# bmt_select_resid vs bmt_select
set.seed(2)
ok_resid <- sapply(1:20, \(i) {
  Tn <- 300; N <- 60
  X  <- matrix(rnorm(Tn * N), Tn, N) + rnorm(Tn) * 0.5
  if (i > 10) X[, 3] <- 1   # constant column
  Xc <- matrix(rnorm(Tn * 3), Tn, 3)
  y  <- drop(1 + X[, 2] - 0.6 * X[, 7] + 0.3 * X[, 30] + Xc %*% c(0.3, -0.2, 0.1) + rnorm(Tn))
  q  <- qr(cbind(1, Xc))
  a  <- bmt_select(y, X, Xc)
  b  <- seq_len(N) %in% bmt_select_resid(qr.resid(q, X), qr.resid(q, y), Tn - q$rank,
                                          colSums(X^2))
  identical(a, b)
})
cat("residual version:", sum(ok_resid), "of", length(ok_resid), "identical\n")
stopifnot(all(ok_resid))
cat("all good\n")
