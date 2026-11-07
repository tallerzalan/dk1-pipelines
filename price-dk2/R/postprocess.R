# bias correction from past errors
# bias: median error, last 28 days
# mz: a + b * forecast, same days

library(dplyr)

k_bias <- 28

bias_day <- function(x, D) {
  past <- x[x$date >= D - k_bias & x$date < D, ]
  if (n_distinct(past$date) < 0.8 * k_bias) return(NULL)
  ab <- coef(lm(actual ~ forecast, data = past))   # mincer-zarnowitz (theil)
  tibble(date = D, bias = median(past$actual - past$forecast), mz_a = ab[[1]], mz_b = ab[[2]])
}

postprocess <- function(fc, m) {
  x   <- select(fc, date, hour, actual, forecast = all_of(m))
  adj <- bind_rows(lapply(sort(unique(x$date)), \(D) bias_day(x, D)))
  x |>
    inner_join(adj, by = "date") |>
    mutate(model        = m,
           corrected    = forecast + bias,
           corrected_mz = mz_a + mz_b * forecast)
}
