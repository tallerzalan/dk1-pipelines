# revenue at risk for the ensemble's battery plan, runs daily after 11_battery.R
# scenarios: past days' 24-hour errors through the day's plan, four ways (hs, fhs, normal, ewma)
# rule set before running: calibrated = kupiec, christoffersen and acerbi-szekely p >= 0.05, then lowest fz0
# ewma was added after the other three failed, so the live days are its real test

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
source("R/lear.R")
source("R/battery.R")
source("R/forecasts.R")
source("R/quantiles.R")   # vol_scale(), err_matrix()
source("R/tests.R")       # dm_test()
source("R/risk.R")

set.seed(1)
lp_def <- battery_lp()

prices <- readRDS("data/processed/prices_dk1_hourly.rds")
daily  <- build_daily(prices)
P      <- as.matrix(daily[, -1])
actual <- tibble(date = rep(daily$date, each = 24), hour = rep(0:23, times = nrow(P)), actual = as.vector(t(P)))

# days with forecast and prices, plus the latest forecast
fc <- all_forecasts("ensemble")
live <- read_csv("forecasts/forecasts_live.csv", show_col_types = FALSE,
                 col_types = cols(forecast_made_utc = col_character(), date = col_date(),
                                  hour = col_integer(), .default = col_double())) |>
  add_ensembles()
next_day <- max(live$date)
next_fc  <- live |> filter(date == next_day) |> arrange(hour)
if (nrow(next_fc) != 24 || anyNA(next_fc$ensemble)) next_fc <- NULL

em  <- err_matrix(transmute(fc, date, hour, e = actual - ensemble))
s   <- vol_scale(actual, unique(c(em$dates, next_day)))
sig <- ewma_vol(em$E)
# for a day after the last known errors: one more ewma step
sig_for <- function(D) {
  i <- match(D, em$dates)
  if (!is.na(i)) return(sig[i])
  j <- max(which(em$dates < D))
  sqrt(lambda_r * sig[j]^2 + (1 - lambda_r) * mean(em$E[j, ]^2))
}

plan_day <- function(f) battery_schedule(f, lp_def)

risk_day <- function(D, q, planned) {
  past <- em$dates >= D - k_r & em$dates < D & complete.cases(em$E)
  sp   <- s$s[match(em$dates, s$date)]
  past <- past & !is.na(sp)
  sD   <- s$s[s$date == D]
  if (sum(past) < 0.8 * k_r || length(sD) == 0 || is.na(sD)) return(NULL)
  long <- em$dates >= D - k_r_long & em$dates < D & complete.cases(em$E)
  adj <- list(hs   = drop(em$E[past, , drop = FALSE] %*% q),
              fhs  = drop((em$E[past, , drop = FALSE] / sp[past]) %*% q) * sD,
              ewma = drop((em$E[long, , drop = FALSE] / sig[long]) %*% q) * sig_for(D))
  st  <- rbind(hs     = tail_stats(planned + adj$hs),
               fhs    = tail_stats(planned + adj$fhs),
               normal = tail_stats_normal(planned + mean(adj$hs), sd(adj$hs)),
               ewma   = tail_stats(planned + adj$ewma))
  list(stats = tibble(date = D, method = rownames(st), planned = planned, as_tibble(st)),
       # shortfalls below expected revenue, for the es test
       scen = list(hs = mean(adj$hs) - adj$hs, fhs = mean(adj$fhs) - adj$fhs, normal = sd(adj$hs),
                   ewma = mean(adj$ewma) - adj$ewma))
}

# every day: plan on the forecast, revenue at the actual prices
days <- sort(unique(fc$date))
out <- lapply(days, \(D) {
  x <- fc[fc$date == D, ] |> arrange(hour)
  q <- plan_day(x$ensemble)
  r <- risk_day(D, q, sum(x$ensemble * q))
  if (is.null(r)) return(NULL)
  r$stats$realized <- sum(x$actual * q)
  r$stats$source   <- x$source[1]
  r
})
out  <- Filter(Negate(is.null), out)
risk <- bind_rows(lapply(out, `[[`, "stats")) |>
  mutate(var = expected - floor, es = expected - tail, miss = realized < floor)
scen <- lapply(names(r_methods), \(m) lapply(out, \(o) o$scen[[m]])) |> setNames(names(r_methods))

cat("revenue at risk:", format(min(risk$date)), "to", format(max(risk$date)), "-", n_distinct(risk$date), "days\n")

# tests per method
tests <- bind_rows(lapply(names(r_methods), \(m) {
  x  <- filter(risk, method == m) |> arrange(date)
  sh <- x$expected - x$realized
  draw <- if (m == "normal") \() unlist(scen[[m]]) * rnorm(nrow(x)) else
    \() vapply(scen[[m]], \(v) v[sample.int(length(v), 1)], numeric(1))
  z <- z2_test(sh, x$var, x$es, draw)
  tibble(method = m, days = nrow(x), misses = sum(x$miss), miss_rate = mean(x$miss),
         kupiec_p = kupiec(x$miss), christoffersen_p = christoffersen(x$miss),
         z2 = z[["z2"]], z2_p = z[["p"]],
         fz0 = mean(fz0(x$realized - x$planned, x$floor - x$planned, x$tail - x$planned)),
         var = mean(x$var), es = mean(x$es),
         live_days = sum(x$source == "live"), live_misses = sum(x$miss & x$source == "live"))
}))

# fz0 by day for the dm test, all scored from the planned revenue
fz_day <- risk |>
  mutate(fz = fz0(realized - planned, floor - planned, tail - planned)) |>
  select(date, method, fz) |>
  pivot_wider(names_from = method, values_from = fz)
best  <- tests$method[which.min(tests$fz0)]
tests <- tests |>
  mutate(dm_p       = sapply(method, \(m) if (m == best) NA_real_ else dm_test(fz_day[[m]] - fz_day[[best]])[["p"]]),
         calibrated = kupiec_p >= 0.05 & coalesce(christoffersen_p, 1) >= 0.05 & z2_p >= 0.05,
         used       = if (any(calibrated)) method == method[calibrated][which.min(fz0[calibrated])]
                      else method == best,
         label      = unname(r_methods[method]))
print(as.data.frame(tests))

# the latest plan, before its auction
nxt <- NULL
if (!is.null(next_fc)) {
  q <- plan_day(next_fc$ensemble)
  r <- risk_day(next_day, q, sum(next_fc$ensemble * q))
  if (!is.null(r)) nxt <- r$stats |> mutate(var = expected - floor, es = expected - tail,
                                            used = method == tests$method[tests$used])
}
if (!is.null(nxt)) print(as.data.frame(nxt))

dir.create("results", showWarnings = FALSE)
write_csv(mutate(risk, across(c(planned, expected, floor, tail, realized, var, es), \(v) round(v, 2))),
          "results/battery_risk_daily.csv")
write_csv(tests, "results/battery_risk_tests.csv")
f_next <- "results/battery_risk_next.csv"
if (!is.null(nxt)) {
  write_csv(mutate(nxt, across(c(planned, expected, floor, tail, var, es), \(v) round(v, 2))), f_next)
} else if (file.exists(f_next)) file.remove(f_next)   # no stale numbers on the dashboard
