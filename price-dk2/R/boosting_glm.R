# boosting with multiple testing (kapetanios, sarafidis & ventouri), returns which columns of X are picked
boosting_glm <- function(y,
                         X,
                         Xcond = NULL,
                         pval = 0.05,
                         delta1 = 1,
                         delta2 = 1,
                         link = c("logit", "probit", "mean"),
                         maxit = 50) {

  link <- match.arg(link)

  # input checks
  if (!is.numeric(y)) stop("y must be numeric.")
  y <- as.numeric(y)

  if (!is.matrix(X)) X <- as.matrix(X)
  if (!is.numeric(X)) stop("X must be numeric.")
  Tn <- nrow(X)
  N  <- ncol(X)

  if (length(y) != Tn)
    stop("length(y) must equal nrow(X).")

  if (is.null(Xcond)) {
    Xcond <- matrix(numeric(0), nrow = Tn, ncol = 0)
  } else {
    if (!is.matrix(Xcond)) Xcond <- as.matrix(Xcond)
    if (!is.numeric(Xcond)) stop("Xcond must be numeric.")
    if (nrow(Xcond) != Tn)
      stop("nrow(Xcond) must equal nrow(X).")
  }

  if (!is.numeric(pval) || length(pval) != 1 || pval <= 0 || pval >= 1)
    stop("pval must be a scalar in (0,1).")

  if (!is.numeric(delta1) || length(delta1) != 1 || is.na(delta1))
    stop("delta1 must be a numeric scalar.")
  if (!is.numeric(delta2) || length(delta2) != 1 || is.na(delta2))
    stop("delta2 must be a numeric scalar.")

  if (delta1 < 1) stop("delta1 must be greater than or equal to 1.")
  if (delta2 < 1) stop("delta2 must be greater than or equal to 1.")

  # thresholds
  p_val1 <- pval / (N^(delta1 - 1))
  t_threshold1 <- stats::qnorm(1 - p_val1 / (2 * N))

  p_val2 <- pval / (N^(delta2 - 1))
  t_threshold2 <- stats::qnorm(1 - p_val2 / (2 * N))

  # abs t/z-stat of the first column
  abs_t_first <- function(Xcand, Xsel, Xcond_local) {

    Xr <- cbind(Xcand, Xsel, Xcond_local)
    df <- data.frame(y = y, Xr)
    colnames(df) <- c("y", paste0("V", seq_len(ncol(Xr))))

    fml <- stats::as.formula(
      paste0("y ~ ", paste(colnames(df)[-1], collapse = " + "))
    )

    if (link %in% c("logit", "probit")) {

      fam <- if (link == "logit") stats::binomial("logit") else stats::binomial("probit")

      fit <- stats::glm(fml, data = df, family = fam,
                        control = stats::glm.control(maxit = maxit))

      sm <- summary(fit)

      if (!("V1" %in% rownames(sm$coefficients))) return(NA_real_)
      return(abs(sm$coefficients["V1", "z value"]))
    }

    # mean link = ols
    fit <- stats::lm(fml, data = df)
    sm <- summary(fit)

    if (!("V1" %in% rownames(sm$coefficients))) return(NA_real_)
    abs(sm$coefficients["V1", "t value"])
  }

  # initial stage
  ind <- rep(FALSE, N)
  ts  <- numeric(N)

  for (i in seq_len(N)) {
    ts[i] <- abs_t_first(X[, i, drop = FALSE], X[, ind, drop = FALSE], Xcond)
  }

  max_i <- which.max(ts)
  if (is.finite(ts[max_i]) && ts[max_i] > t_threshold1)
    ind[max_i] <- TRUE

  # boosting loop
  repeat {

    ts <- rep(-Inf, N)

    for (i in seq_len(N)) {
      if (!ind[i]) {
        ts[i] <- abs_t_first(X[, i, drop = FALSE], X[, ind, drop = FALSE], Xcond)
      }
    }

    max_i <- which.max(ts)
    if (is.finite(ts[max_i]) && ts[max_i] > t_threshold2) {
      ind[max_i] <- TRUE
    } else {
      break
    }
  }

  ind
}
