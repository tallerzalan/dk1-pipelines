# bmt-x and lear-x, one day

# candidates for day d
x_inputs <- function(features, exog, d, groups, window = 730) {
  P     <- features$P
  train <- (d - window):(d - 1)
  med <- median(P[train, ], na.rm = TRUE)
  s   <- mad(P[train, ], na.rm = TRUE)

  ex <- transform_exog(exog, train)[groups]
  X  <- cbind(transform_p(features$X_price, med, s), do.call(cbind, unname(ex)))
  if (anyNA(X[d, ])) stop("Missing inputs for the forecast day")

  keep <- train[complete.cases(X[train, ]) & complete.cases(P[train, ])]
  list(X = X, keep = keep, med = med, s = s)
}

bmtx_forecast_day <- function(features, exog, d, groups = c("nb", "hol", "wx"), window = 730,
                              pval = 0.05, delta1 = 1, delta2 = 1) {
  inp <- x_inputs(features, exog, d, groups, window)
  D   <- features$X_dow[, -1]   # monday = baseline
  Xtr <- inp$X[inp$keep, ]
  Dtr <- D[inp$keep, ]

  qC  <- qr(cbind(1, Dtr))
  E0  <- qr.resid(qC, Xtr)
  df0 <- length(inp$keep) - qC$rank
  ss0 <- colSums(Xtr^2)

  sapply(1:24, \(h) {
    y   <- transform_p(features$P[inp$keep, h], inp$med, inp$s)
    sel <- bmt_select_resid(E0, qr.resid(qC, y), df0, ss0, pval, delta1, delta2)
    beta <- qr.coef(qr(cbind(1, Dtr, Xtr[, sel, drop = FALSE])), y)
    beta[is.na(beta)] <- 0
    inverse_p(sum(c(1, D[d, ], inp$X[d, sel]) * beta), inp$med, inp$s)
  })
}

learx_forecast_day <- function(features, exog, d, groups = c("nb", "hol", "wx"), window = 730) {
  inp <- x_inputs(features, exog, d, groups, window)
  Xl  <- cbind(inp$X[inp$keep, ], features$X_dow[inp$keep, ])
  xl  <- matrix(c(inp$X[d, ], features$X_dow[d, ]), nrow = 1)

  sapply(1:24, \(h) {
    y <- transform_p(features$P[inp$keep, h], inp$med, inp$s)
    m <- fit_lasso_bic(Xl, y)
    as.numeric(inverse_p(predict(m$fit, xl, s = m$lambda), inp$med, inp$s))
  })
}
