fit_gaussian <- function(y, X, pred_other) {
  # create residuals to compute conditional params
  y_res <- y - pred_other
  b_hat <- solve(crossprod(X), crossprod(X, y_res))
  return(c(b_hat))
}

fit_binomial <- function(y, X, pred_other, pred_self) {
  # create working response for IRLS update
  pred  <- pred_other + pred_self
  prob  <- 1 / (1 + exp(-pred))
  wght  <- prob * (1 - prob)
  eps   <- .Machine$double.eps * 10
  if (any(wght < eps) || any(wght > 1 - eps))
    wght <- vapply(wght, function(w) min(max(eps, w), 1 - eps), 0.5)
  work  <- pred + (y - prob) / wght
  y_res <- work - pred_other
  W     <- diag(c(wght))
  b_hat <- solve(crossprod(X, W %*% X), crossprod(X, W %*% y_res))
  return(c(b_hat))
}
