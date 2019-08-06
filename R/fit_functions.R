fit_gaussian <- function(y, X, pred_other) {
  # create residuals to compute conditional params
  y_res <- y - pred_other
  b_hat <- stats::.lm.fit(X, y_res)[["coefficients"]]
  return(c(b_hat))
}

ll_gaussian <- function(b, y, X, pred_other) {
  N      <- length(y)
  res    <- y - pred_other - X %*% b
  ssr    <- c(crossprod(res))
  sig2   <- ssr / N
  -N / 2 * log(2 * pi) - N * log(sqrt(sig2)) - 1 / (2 * sig2) * ssr
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
  W     <- Matrix::sparseMatrix(i = 1:length(wght), j = 1:length(wght), x = c(wght))
  b_hat <- Matrix::solve(Matrix::crossprod(X, W %*% X), Matrix::crossprod(X, W %*% y_res))
  return(as.vector(b_hat))
}

