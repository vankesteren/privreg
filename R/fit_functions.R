fit_gaussian <- function(y, X, pred_other) {
  # create residuals to compute conditional params
  y_res <- y - pred_other
  b_hat <- stats::.lm.fit(X, y_res)[["coefficients"]]
  return(c(b_hat))
}

fit_binomial <- function(y, X, pred_other) {
  fit <- stats::glm.fit(X, y, offset = pred_other, family = binomial(),
                        intercept = FALSE)
  return(as.vector(fit[["coefficients"]]))
}
