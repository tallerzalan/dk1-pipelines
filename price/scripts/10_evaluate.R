# evaluation, runs daily on github
# backtest csv rebuilt locally

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
source("R/lear.R")
source("R/mcs.R")
source("R/forecasts.R")
source("R/postprocess.R")
source("R/quantiles.R")
source("R/tests.R")

# models on the dashboard
models <- c("ensemble", "bmtx", "learx", "xgbx", "naive_mix")
# kept in backtest file
stored <- c("bmtx", "learx", "xgbx", "bmt", "lear", "xgb", "naive_day", "naive_mix")
dir.create("results", showWarnings = FALSE)

prices <- readRDS("data/processed/prices_dk1_hourly.rds")
daily  <- build_daily(prices)
P      <- as.matrix(daily[, -1])
actual <- tibble(date = rep(daily$date, each = 24), hour = rep(0:23, times = nrow(P)),
                 actual = as.vector(t(P)))


# rebuild backtest forecasts
bt_files <- paste0("data/processed/forecasts_", c("lear", "bmt", "xgb", "weather_de"), ".rds")
wide <- function(x) x |> select(date, hour, model, forecast) |>
  pivot_wider(names_from = model, values_from = forecast)
complete_days <- function(x) x |> drop_na() |> group_by(date) |> filter(n() == 24) |> ungroup()
if (all(file.exists(bt_files))) {
  backtest <- readRDS(bt_files[1]) |>
    select(date, hour, actual, lear, naive_mix) |>
    inner_join(select(readRDS(bt_files[2]), date, hour, bmt), by = c("date", "hour")) |>
    # -x models with dk1 and german weather (09d)
    inner_join(wide(readRDS(bt_files[4])), by = c("date", "hour")) |>
    # plain xgboost
    inner_join(wide(filter(readRDS(bt_files[3]), model == "xgb")), by = c("date", "hour")) |>
    inner_join(transmute(actual, date = date + 1, hour, naive_day = actual), by = c("date", "hour")) |>
    complete_days() |>
    select(date, hour, actual, all_of(stored)) |>
    arrange(date, hour)
  write_csv(mutate(backtest, across(-c(date, hour), \(x) round(x, 2))), "results/backtest_forecasts.csv")
} else if (file.exists(bt_files[4])) {
  # only 09d rerun: swap in the new -x models
  backtest <- read_csv("results/backtest_forecasts.csv", show_col_types = FALSE) |>
    select(-any_of(c("bmtx", "learx", "xgbx"))) |>
    inner_join(wide(readRDS(bt_files[4])), by = c("date", "hour")) |>
    select(date, hour, actual, all_of(stored)) |>
    complete_days() |>
    arrange(date, hour)
  write_csv(mutate(backtest, across(-c(date, hour), \(x) round(x, 2))), "results/backtest_forecasts.csv")
}
fc <- all_forecasts(models)
backtest <- filter(fc, source == "backtest")
live     <- filter(fc, source == "live")

cat("evaluation period:", format(min(fc$date)), "to", format(max(fc$date)), "-",
    n_distinct(backtest$date), "backtest days and", n_distinct(live$date), "live days\n")


# accuracy, rmae vs weekly naive
mae_naive <- mean(abs(fc$actual - fc$naive_mix))
accuracy <- tibble(model = models) |>
  rowwise() |>
  mutate(mae  = mean(abs(fc$actual - fc[[model]])),
         rmse = sqrt(mean((fc$actual - fc[[model]])^2)),
         rmae = mae / mae_naive) |>
  ungroup() |>
  mutate(n_hours = nrow(fc), n_days = n_distinct(fc$date),
         from = min(fc$date), to = max(fc$date),
         n_backtest_days = n_distinct(fc$date[fc$source == "backtest"]),
         backtest_to     = max(fc$date[fc$source == "backtest"]),
         n_live_days     = n_distinct(fc$date[fc$source == "live"])) |>
  arrange(mae)
print(accuracy)


# tests on daily loss differential
# negative = a more accurate
daily_loss <- function(m, loss = c("abs", "sq")) {
  loss <- match.arg(loss)
  e <- fc$actual - fc[[m]]
  l <- if (loss == "abs") abs(e) else e^2
  as.numeric(tapply(l, fc$date, mean))
}

gw_test <- function(d) {
  Z  <- cbind(1, head(d, -1)) * tail(d, -1)
  n  <- nrow(Z)
  zb <- colMeans(Z)
  stat <- n * drop(t(zb) %*% solve(crossprod(Z) / n) %*% zb)
  c(stat = stat, p = pchisq(stat, df = ncol(Z), lower.tail = FALSE))
}

# hours better / worse, holm over 24 hours
hour_tests <- function(a, b) {
  r <- sapply(0:23, \(h) {
    x <- fc[fc$hour == h, ]
    dm_test(abs(x$actual - x[[a]]) - abs(x$actual - x[[b]]))
  })
  sig <- p.adjust(r["p", ], "holm") < 0.05
  c(hours_a_better = sum(sig & r["stat", ] < 0),
    hours_b_better = sum(sig & r["stat", ] > 0))
}

pairs <- combn(models, 2, simplify = FALSE)
tests <- bind_rows(lapply(pairs, \(ab) {
  a <- ab[1]; b <- ab[2]
  d_abs <- daily_loss(a, "abs") - daily_loss(b, "abs")
  d_sq  <- daily_loss(a, "sq")  - daily_loss(b, "sq")
  dm  <- dm_test(d_abs)
  dm2 <- dm_test(d_sq)
  gw  <- gw_test(d_abs)
  ht  <- hour_tests(a, b)
  tibble(model_a = a, model_b = b,
         mae_diff = mean(d_abs),
         dm_stat = dm[["stat"]], dm_p = dm[["p"]],
         dm_sq_stat = dm2[["stat"]], dm_sq_p = dm2[["p"]],
         gw_stat = gw[["stat"]], gw_p = gw[["p"]],
         hours_a_better = ht[["hours_a_better"]], hours_b_better = ht[["hours_b_better"]])
}))
print(select(tests, model_a, model_b, mae_diff, dm_p, gw_p, hours_a_better, hours_b_better))


# bias: mean error and mz
# newey-west, one week of hours
nw_vcov <- function(X, e, lag = 168) {
  n <- nrow(X)
  u <- X * e
  S <- crossprod(u) / n
  for (k in 1:lag) {
    G <- crossprod(u[-(1:k), , drop = FALSE], u[1:(n - k), , drop = FALSE]) / n
    S <- S + (1 - k / (lag + 1)) * (G + t(G))
  }
  B <- solve(crossprod(X) / n)
  B %*% S %*% B / n
}

bias <- bind_rows(lapply(models, \(m) {
  X    <- cbind(1, fc[[m]])
  y    <- fc$actual
  beta <- drop(solve(crossprod(X), crossprod(X, y)))
  V    <- nw_vcov(X, drop(y - X %*% beta))
  r    <- beta - c(0, 1)
  wald <- drop(t(r) %*% solve(V) %*% r)
  tibble(model = m,
         mean_error   = mean(fc$actual - fc[[m]]),
         mz_intercept = beta[1],
         mz_slope     = beta[2],
         mz_p         = pchisq(wald, df = 2, lower.tail = FALSE))
}))
print(bias)


# cheapest and peak hour
shape <- bind_rows(lapply(models, \(m) {
  fc |>
    group_by(date) |>
    summarise(cheap_true = hour[which.min(actual)], cheap_fc = hour[which.min(.data[[m]])],
              peak_true  = hour[which.max(actual)], peak_fc  = hour[which.max(.data[[m]])],
              spearman   = cor(actual, .data[[m]], method = "spearman"),
              .groups = "drop") |>
    summarise(model          = m,
              cheapest_exact = mean(cheap_true == cheap_fc),
              cheapest_pm1   = mean(abs(cheap_true - cheap_fc) <= 1),
              peak_exact     = mean(peak_true == peak_fc),
              peak_pm1       = mean(abs(peak_true - peak_fc) <= 1),
              spearman       = mean(spearman, na.rm = TRUE))
}))
print(shape)


# model confidence set
L_models <- sapply(models, \(m) daily_loss(m, "abs"))
mcs_models <- mcs(L_models) |>
  mutate(in_mcs = mcs_p >= 0.10)
print(mcs_models)

# bias correction
# mz vs raw and median
post <- bind_rows(lapply(setdiff(models, "naive_mix"), \(m) {
  x  <- postprocess(fc, m)
  dl <- x |>
    group_by(date) |>
    summarise(raw = mean(abs(actual - forecast)),
              med = mean(abs(actual - corrected)),
              mz  = mean(abs(actual - corrected_mz)), .groups = "drop")
  dm        <- dm_test(dl$med - dl$raw)
  dm_mz     <- dm_test(dl$mz - dl$raw)
  dm_mz_med <- dm_test(dl$mz - dl$med)
  tibble(model           = m,
         days            = n_distinct(x$date),
         mae_raw         = mean(abs(x$actual - x$forecast)),
         mae_corr        = mean(abs(x$actual - x$corrected)),
         dm_p            = dm[["p"]],
         mean_error_raw  = mean(x$actual - x$forecast),
         mean_error_corr = mean(x$actual - x$corrected),
         mae_mz          = mean(abs(x$actual - x$corrected_mz)),
         mean_error_mz   = mean(x$actual - x$corrected_mz),
         dm_p_mz         = dm_mz[["p"]],
         dm_p_mz_med     = dm_mz_med[["p"]],
         mz_slope_median = median(x$mz_b))
}))
print(post)
write_csv(post, "results/eval_post.csv")


# quantile forecasts, the naive with the same methods is the benchmark
real  <- setdiff(models, "naive_mix")
q_models <- c(real, "naive_mix")
scale <- vol_scale(bind_rows(select(fc, date, hour, actual), actual), sort(unique(fc$date)))
qf <- bind_rows(lapply(q_models, \(m) {
  em <- err_matrix(transmute(fc, date, hour, e = actual - .data[[m]]))
  quantile_offsets(em$dates, em$E, scale$s[match(em$dates, scale$date)]) |>
    mutate(model = m)
})) |>
  inner_join(fc |>
               select(date, hour, source, actual, all_of(q_models)) |>
               pivot_longer(all_of(q_models), names_to = "model", values_to = "forecast"),
             by = c("model", "date", "hour")) |>
  mutate(q = forecast + off, below = actual < q, loss = pinball(actual, q, tau))

# per day: pinball, coverage, width
q_day <- qf |>
  filter(tau %in% c(0.05, 0.25, 0.75, 0.95)) |>
  select(model, method, date, hour, source, actual, tau, q) |>
  pivot_wider(names_from = tau, values_from = q, names_prefix = "q") |>
  group_by(model, method, date, source) |>
  summarise(in50 = mean(actual >= q0.25 & actual <= q0.75),
            in90 = mean(actual >= q0.05 & actual <= q0.95),
            w50  = mean(q0.75 - q0.25),
            w90  = mean(q0.95 - q0.05), .groups = "drop") |>
  inner_join(qf |>
               group_by(model, method, date) |>
               summarise(pinball = mean(loss), .groups = "drop"),
             by = c("model", "method", "date")) |>
  arrange(model, method, date)

# share below each quantile
calib <- qf |>
  group_by(model, method, tau) |>
  summarise(below = mean(below), .groups = "drop")

# coverage p: newey-west, daily
# chosen: calibrated first, then pinball
prob <- q_day |>
  group_by(model, method) |>
  summarise(days       = n(),
            pinball    = mean(pinball),
            cov50      = mean(in50),
            cov90      = mean(in90),
            width50    = mean(w50),
            width90    = mean(w90),
            cov50_p    = nw_test(in50, 0.5)[["p"]],
            cov90_p    = nw_test(in90, 0.9)[["p"]],
            days_live  = sum(source == "live"),
            cov50_live = if (days_live > 0) mean(in50[source == "live"]) else NA_real_,
            cov90_live = if (days_live > 0) mean(in90[source == "live"]) else NA_real_,
            .groups = "drop") |>
  inner_join(calib |>
               group_by(model, method) |>
               summarise(cal_error = mean(abs(below - tau)), .groups = "drop"),
             by = c("model", "method")) |>
  group_by(model) |>
  mutate(best = method[which.min(pinball)],
         dm_p = sapply(method, \(me) {
           if (me == best[1]) return(NA_real_)
           a <- q_day$pinball[q_day$model == model[1] & q_day$method == me]
           b <- q_day$pinball[q_day$model == model[1] & q_day$method == best[1]]
           dm_test(a - b)[["p"]]
         }),
         calibrated = cov50_p >= 0.05 & cov90_p >= 0.05,
         chosen = if (any(calibrated)) method == method[calibrated][which.min(pinball[calibrated])]
         else method == method[which.min(cal_error)]) |>
  ungroup()

# skill against the naive forecast's error quantiles, same days
naive_q <- q_day |> filter(model == "naive_mix", method == "emp") |> select(date, pinball_naive = pinball)
prob <- prob |>
  left_join(q_day |>
              inner_join(naive_q, by = "date") |>
              group_by(model, method) |>
              summarise(skill   = 1 - mean(pinball) / mean(pinball_naive),
                        skill_p = dm_test(pinball - pinball_naive)[["p"]], .groups = "drop"),
            by = c("model", "method"))
print(prob)

# chance of a negative price, brier score vs the 28-day share in that hour
neg_clim <- actual |>
  arrange(date) |>
  group_by(hour) |>
  mutate(clim = sapply(seq_along(date), \(i) {
    past <- date >= date[i] - 28 & date < date[i]
    if (sum(past) < 20) NA_real_ else mean(actual[past] < 0)
  })) |>
  ungroup() |>
  select(date, hour, clim)
neg <- qf |>
  semi_join(filter(prob, chosen), by = c("model", "method")) |>
  group_by(model, method, date, hour, actual) |>
  summarise(p = prob_below(sort(q), tau[order(q)]), .groups = "drop") |>
  inner_join(neg_clim, by = c("date", "hour")) |>
  filter(!is.na(clim)) |>
  mutate(y = as.numeric(actual < 0))
neg_scores <- neg |>
  group_by(model, method) |>
  summarise(hours = n(), neg_hours = sum(y), brier = mean((p - y)^2), brier_clim = mean((clim - y)^2),
            .groups = "drop") |>
  mutate(skill = 1 - brier / brier_clim)
print(neg_scores)
write_csv(neg_scores, "results/prob_negative.csv")
write_csv(prob,  "results/prob_scores.csv")
write_csv(calib, "results/prob_calibration.csv")

# challengers: replace the ensemble only if live mae is lower with p < 0.05 (newey-west dm, holm)
# rule fixed 12 oct 2026
challenger_start    <- as_date("2026-10-14")
challenger_decision <- as_date("2027-03-01")
challengers <- c(ensemble_bc = "Bias-corrected")

bt_ch <- read_csv("results/backtest_forecasts.csv", show_col_types = FALSE) |> add_ensembles()
bc_bt <- postprocess(transmute(bt_ch, date, hour, actual, ensemble), "ensemble") |>
  transmute(date, hour, ensemble_bc = corrected)
bt_ch <- left_join(bt_ch, bc_bt, by = c("date", "hour"))

live_ch <- read_csv("forecasts/forecasts_live.csv", show_col_types = FALSE,
                    col_types = cols(forecast_made_utc = col_character(), date = col_date(),
                                     hour = col_integer(), .default = col_double())) |>
  filter(date >= challenger_start, date < challenger_decision) |>
  inner_join(actual, by = c("date", "hour"))

compare <- function(x, ch) {
  if (!ch %in% names(x)) return(tibble(days = 0L))
  d <- x |>
    filter(!is.na(ensemble), !is.na(.data[[ch]])) |>
    group_by(date) |>
    filter(n() == 24) |>
    summarise(champion = mean(abs(actual - ensemble)), challenger = mean(abs(actual - .data[[ch]])),
              .groups = "drop")
  if (nrow(d) < 2) return(tibble(days = nrow(d)))
  tibble(days = nrow(d), mae_champion = mean(d$champion), mae_challenger = mean(d$challenger),
         diff = mean(d$challenger - d$champion), p = dm_test(d$challenger - d$champion)[["p"]])
}
challenger_tbl <- bind_rows(lapply(names(challengers), \(ch) {
  b <- compare(bt_ch, ch); l <- compare(live_ch, ch)
  bind_cols(tibble(challenger = ch, label = challengers[[ch]]),
            rename_with(b, \(n) paste0("backtest_", n)), rename_with(l, \(n) paste0("live_", n)))
}))
for (v in c(paste0("backtest_", c("mae_champion", "mae_challenger", "diff", "p")),
            paste0("live_", c("mae_champion", "mae_challenger", "diff", "p")))) {
  if (!v %in% names(challenger_tbl)) challenger_tbl[[v]] <- NA_real_
}
challenger_tbl <- challenger_tbl |>
  mutate(live_p_holm = p.adjust(live_p, "holm"),
         status = case_when(Sys.Date() < challenger_decision ~ "running",
                            !is.na(live_p_holm) & live_diff < 0 & live_p_holm < 0.05 ~ "replaces the ensemble",
                            TRUE ~ "ensemble kept"),
         start = challenger_start, decision = challenger_decision)
print(as.data.frame(challenger_tbl))
write_csv(challenger_tbl, "results/challengers.csv")

write_csv(accuracy, "results/eval_accuracy.csv")
write_csv(tests,    "results/eval_tests.csv")
write_csv(bias,     "results/eval_bias.csv")
write_csv(shape,    "results/eval_shape.csv")
write_csv(mcs_models, "results/mcs_models.csv")