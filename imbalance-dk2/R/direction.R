# direction models, all return a matrix with columns down, none, up

library(glmnet)

classes <- c(down = -1, none = 0, up = 1)

# benchmark: share of each direction in that hour over the last 28 days (+0.5 smoothing)
clim_probs <- function(train, test, days = 28) {
  last <- max(train$date)
  counts <- train |>
    filter(date > last - days) |>
    count(hour, dir) |>
    complete(hour = 0:23, dir = classes, fill = list(n = 0)) |>
    mutate(n = n + 0.5) |>
    group_by(hour) |>
    mutate(p = n / sum(n)) |>
    ungroup() |>
    select(hour, dir, p) |>
    pivot_wider(names_from = dir, values_from = p)
  P <- as.matrix(counts[match(test$hour, counts$hour), as.character(classes)])
  colnames(P) <- names(classes)
  P
}

# multinomial lasso, lambda by bic. fit and predict apart, so live can reuse the weekly fit
da_extra <- c("morning_up", "morning_down", "week_up", "week_down")

fit_mnl <- function(train, extra = da_extra) {
  y   <- factor(train$dir, levels = classes)
  fit <- glmnet(model_matrix(train, extra), y, family = "multinomial", type.multinomial = "grouped")
  dev <- (1 - fit$dev.ratio) * fit$nulldev
  bic <- dev + log(nrow(train)) * fit$df * (length(classes) - 1)
  list(fit = fit, lambda = fit$lambda[which.min(bic)], extra = extra)
}
pred_mnl <- function(m, test) {
  P <- predict(m$fit, model_matrix(test, m$extra), s = m$lambda, type = "response")[, , 1, drop = FALSE][, , 1]
  if (is.null(dim(P))) P <- matrix(P, nrow = 1)
  colnames(P) <- names(classes)
  P
}
mnl_probs <- function(train, test, extra = da_extra) pred_mnl(fit_mnl(train, extra), test)

# xgboost, fixed settings, seed = fit date
xgb_dir_params <- list(objective = "multi:softprob", num_class = 3, learning_rate = 0.05,
                       max_depth = 4, min_child_weight = 20, subsample = 0.8,
                       colsample_bytree = 0.8, nthread = 1)
xgb_dir_rounds <- 300

fit_xgb <- function(train, seed, cols = feature_cols) {
  set.seed(seed)
  m <- xgb.train(params = xgb_dir_params,
                 data = xgb.DMatrix(as.matrix(train[, cols]), label = match(train$dir, classes) - 1),
                 nrounds = xgb_dir_rounds, verbose = 0)
  list(raw = xgb.save.raw(m), cols = cols)
}
pred_xgb <- function(m, test) {
  P <- predict(xgb.load.raw(m$raw), as.matrix(test[, m$cols]))
  if (is.null(dim(P))) P <- matrix(P, ncol = 3, byrow = TRUE)
  colnames(P) <- names(classes)
  P
}
xgb_probs <- function(train, test, seed, cols = feature_cols) pred_xgb(fit_xgb(train, seed, cols), test)

# log loss per quarter, probabilities clipped
log_loss <- function(P, dir) {
  P <- pmax(P, 1e-4); P <- P / rowSums(P)
  -log(P[cbind(seq_along(dir), match(dir, classes))])
}
