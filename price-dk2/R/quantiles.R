# quantile forecasts from past errors
# emp: error quantiles, last 60 days
# emp_s: scaled by last week's sd
# aci: adaptive levels (gibbs & candes)

library(dplyr)

taus      <- round(seq(0.05, 0.95, by = 0.05), 2)
k_q       <- 60     # days of errors
k_vol     <- 7      # days for sd
gamma_q   <- 0.02   # aci step
q_methods <- c(emp = "Error quantiles", emp_s = "Scaled by volatility", aci = "Adaptive (ACI)")

# sd of last week's prices
vol_scale <- function(prices, dates) {
  prices <- distinct(prices, date, hour, .keep_all = TRUE)
  tibble(date = dates,
         s = sapply(dates, \(D) {
           x <- prices$actual[prices$date >= D - k_vol & prices$date < D]
           if (length(x) < 0.8 * 24 * k_vol) NA else sd(x, na.rm = TRUE)
         }))
}

# date, hour, e to days x 24
err_matrix <- function(x) {
  dates <- sort(unique(x$date))
  E <- matrix(NA_real_, length(dates), 24)
  E[cbind(match(x$date, dates), x$hour + 1)] <- x$e
  list(dates = dates, E = E)
}

# offsets from forecast, all methods
# only past days used
quantile_offsets <- function(dates, E, s) {
  level <- taus
  out <- vector("list", length(dates))
  for (i in seq_along(dates)) {
    past <- dates >= dates[i] - k_q & dates < dates[i] & complete.cases(E) & !is.na(s)
    if (sum(past) < 0.8 * k_q || is.na(s[i])) next
    Ep  <- E[past, , drop = FALSE]
    Zp  <- Ep / s[past]
    off <- list(emp   = apply(Ep, 2, quantile, probs = taus, names = FALSE),
                emp_s = s[i] * apply(Zp, 2, quantile, probs = taus, names = FALSE),
                aci   = s[i] * apply(Zp, 2, quantile, probs = level, names = FALSE))
    off <- lapply(off, \(o) apply(o, 2, sort))   # no crossing
    out[[i]] <- bind_rows(lapply(names(off), \(m)
      tibble(date = dates[i], method = m, hour = rep(0:23, each = length(taus)),
             tau = rep(taus, 24), off = as.vector(off[[m]]))))
    # aci update after the day
    if (!anyNA(E[i, ])) {
      hit   <- rowMeans(sweep(off$aci, 2, E[i, ], ">"))
      level <- pmin(pmax(level + gamma_q * (taus - hit), 0.005), 0.995)
    }
  }
  if (all(sapply(out, is.null))) {
    return(tibble(date = as.Date(character()), method = character(), hour = integer(),
                  tau = numeric(), off = numeric()))
  }
  bind_rows(out)
}

pinball <- function(y, q, tau) {
  u <- y - q
  pmax(tau * u, (tau - 1) * u)
}

prob_below <- function(q, tau, x = 0) {
  if (x < min(q)) return(0)
  if (x > max(q)) return(1)
  approx(q, tau, xout = x, ties = mean)$y
}
