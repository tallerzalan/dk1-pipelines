# battery plan for one day
# c charge, d discharge, s stored, u = charging

library(lpSolve)

battery_setup <- list(cap_mwh   = 2,      # MWh
                      power_mw  = 1,      # MW
                      eff_rt    = 0.90,   # round trip
                      max_cycle = 1)      # cycles per day

battery_lp <- function(setup = battery_setup, n = 24) {
  eff <- sqrt(setup$eff_rt)
  ic <- 1:n; id <- n + 1:n; soc <- 2 * n + 1:n; iu <- 3 * n + 1:n
  rows <- list(); dirs <- c(); rhs <- c()
  add <- function(r, dir, b) {
    rows[[length(rows) + 1]] <<- r; dirs <<- c(dirs, dir); rhs <<- c(rhs, b)
  }

  # storage balance, starts empty
  for (h in 1:n) {
    r <- numeric(4 * n)
    r[soc[h]] <- 1
    if (h > 1) r[soc[h - 1]] <- -1
    r[ic[h]] <- -eff
    r[id[h]] <- 1 / eff
    add(r, "=", 0)
  }
  # empty at end of day
  r <- numeric(4 * n); r[soc[n]] <- 1; add(r, "=", 0)

  # limits
  for (h in 1:n) {
    r <- numeric(4 * n); r[soc[h]] <- 1; add(r, "<=", setup$cap_mwh)
    r <- numeric(4 * n); r[ic[h]] <- 1; r[iu[h]] <- -setup$power_mw; add(r, "<=", 0)              # c_h <= power * u_h
    r <- numeric(4 * n); r[id[h]] <- 1; r[iu[h]] <- setup$power_mw; add(r, "<=", setup$power_mw)  # d_h <= power * (1 - u_h)
  }
  # cycle limit, energy out of storage
  r <- numeric(4 * n); r[id] <- 1 / eff; add(r, "<=", setup$cap_mwh * setup$max_cycle)

  list(A = do.call(rbind, rows), dirs = dirs, rhs = rhs, ic = ic, id = id, iu = iu, n = n)
}

# best plan for given prices
battery_schedule <- function(price, lp_def = battery_lp()) {
  n   <- lp_def$n
  obj <- c(-price, price, rep(0, 2 * n))
  sol <- lp("max", obj, lp_def$A, lp_def$dirs, lp_def$rhs, binary.vec = lp_def$iu)
  if (sol$status != 0) stop("lp did not solve")
  sol$solution[lp_def$id] - sol$solution[lp_def$ic]
}

# plan on forecast, paid actual
battery_profit <- function(forecast, actual, lp_def = battery_lp()) {
  sum(actual * battery_schedule(forecast, lp_def))
}
