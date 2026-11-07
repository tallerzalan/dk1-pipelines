# mcs, hansen et al. 2011
# L: daily losses, days x models

library(dplyr)

mcs <- function(L, B = 2000, block = 7, seed = 1) {
  set.seed(seed)
  n <- nrow(L)
  idx <- replicate(B, {
    starts <- sample.int(n, ceiling(n / block), replace = TRUE)
    (as.vector(outer(0:(block - 1), starts, "+")) - 1) %% n + 1
  })[1:n, ]
  boot_means_all <- apply(idx, 2, \(i) colMeans(L[i, , drop = FALSE]))   # models x B

  alive <- colnames(L)
  p_run <- 0
  out   <- tibble(model = character(), mcs_p = numeric())
  while (length(alive) > 1) {
    lbar  <- colMeans(L[, alive, drop = FALSE])
    d_i   <- lbar - mean(lbar)
    bm    <- boot_means_all[alive, , drop = FALSE]
    bd_i  <- sweep(bm, 2, colMeans(bm))                    # bootstrap d_i
    v_i   <- rowMeans((bd_i - d_i)^2)
    t_i   <- d_i / sqrt(v_i)
    if (!any(is.finite(t_i))) break   # identical losses
    t_max <- max(t_i, na.rm = TRUE)
    boot  <- apply((bd_i - d_i) / sqrt(v_i), 2, max, na.rm = TRUE)
    p_run <- max(p_run, mean(boot >= t_max))
    worst <- alive[which.max(t_i)]   # which.max skips NA
    out   <- bind_rows(out, tibble(model = worst, mcs_p = p_run))
    alive <- setdiff(alive, worst)
  }
  bind_rows(out, tibble(model = alive, mcs_p = 1))
}
