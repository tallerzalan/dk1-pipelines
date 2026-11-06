# xgboost, one model all hours

library(xgboost)

xgb_params <- list(objective        = "reg:squarederror",
                   learning_rate    = 0.05,
                   max_depth        = 6,
                   min_child_weight = 10,
                   subsample        = 0.8,
                   colsample_bytree = 0.8,
                   nthread          = 1)
xgb_rounds <- 500

# days x 24 to long
long <- function(M) as.vector(t(M))

# features per day and hour
xgb_design <- function(features, rows, exog = NULL) {
  P   <- features$P
  lag <- function(k) lag_days(P, k)[rows, , drop = FALSE]
  d1  <- lag(1)
  rep24 <- function(x) rep(x, each = 24)

  X <- cbind(
    hour    = rep(0:23, times = length(rows)),
    dow     = rep24(features$dow[rows]),
    p_d1    = long(d1),
    p_d2    = long(lag(2)),
    p_d3    = long(lag(3)),
    p_d7    = long(lag(7)),
    d1_mean = rep24(rowMeans(d1)),
    d1_min  = rep24(apply(d1, 1, min)),
    d1_max  = rep24(apply(d1, 1, max)),
    d1_h23  = rep24(d1[, 24])
  )
  if (is.null(exog)) return(X)

  series <- function(M, name) {
    m <- M[rows, grepl(paste0("^", name, "_"), colnames(M)), drop = FALSE]
    cbind(long(m), rep24(rowMeans(m)))
  }
  nb <- do.call(cbind, lapply(c("de", "dk2", "no2", "se3", "se4"), \(a) {
    m <- series(exog$nb, paste0(a, "_d1"))
    colnames(m) <- paste0(a, c("_d1", "_d1_mean"))
    m
  }))
  # open-meteo weather
  wx <- NULL
  if (!is.null(exog$wx)) {
    wx <- do.call(cbind, lapply(c("wxwind_d0", "wxsolar_d0"), \(v) {
      m <- series(exog$wx, v)
      colnames(m) <- paste0(sub("_d0", "", v), c("", "_mean"))
      m
    }))
  }
  # open-meteo for germany
  wxde <- NULL
  if (!is.null(exog$wxde)) {
    wxde <- do.call(cbind, lapply(c("wxdewind_d0", "wxdesolar_d0"), \(v) {
      m <- series(exog$wxde, v)
      colnames(m) <- paste0(sub("_d0", "", v), c("", "_mean"))
      m
    }))
  }
  cbind(X, nb, wx, wxde,
        hol_d0 = rep24(exog$hol[rows, "hol_d0"]),
        hol_d1 = rep24(exog$hol[rows, "hol_d1"]))
}

# seed = date, so live = backtest
xgb_forecast_day <- function(features, d, window = 730, exog = NULL, params = xgb_params,
                             rounds = xgb_rounds, seed) {
  P     <- features$P
  train <- (d - window):(d - 1)

  med <- median(P[train, ], na.rm = TRUE)
  s   <- mad(P[train, ], na.rm = TRUE)

  X_test <- xgb_design(features, d, exog)
  if (anyNA(X_test)) stop("Missing inputs for the forecast day")

  X <- xgb_design(features, train, exog)
  y <- transform_p(long(P[train, , drop = FALSE]), med, s)
  keep <- !is.na(y)

  set.seed(seed)
  m <- xgb.train(params = params,
                 data = xgb.DMatrix(X[keep, ], label = y[keep]),
                 nrounds = max(rounds), verbose = 0)
  fc <- sapply(rounds, \(r) inverse_p(predict(m, X_test, iterationrange = c(1, r)), med, s))
  if (length(rounds) == 1) drop(fc) else fc
}
