#' Local vertically partitioned data regression
#'
#' Perform privreg locally with two vertically partitioned datasets
#'
#' @param y outcome variable
#' @param Xa alice model matrix
#' @param Xb bob model matrix
#' @param family response family (use family object!)
#' @param se whether to compute the standard error
#' @param tol tolerance
#' @param maxit maximum iterations
#' @param debug print debug information
#'
#' @importFrom stats coef gaussian glm glm.fit vcov
#'
#' @export

privreg_local <- function(y, Xa, Xb, family = gaussian(), se = TRUE,
                          tol = 1e-12, maxit = 1e4, debug = TRUE) {
  conv <- FALSE
  i <- 0L
  N <- length(y)
  Pa <- ncol(Xa)
  Pb <- ncol(Xb)

  # storage matrices
  bhat_a <- matrix(0, Pa, maxit)
  bhat_b <- matrix(0, Pb, maxit)
  eta_a <- matrix(0, N, maxit)
  eta_b <- matrix(0, N, maxit)

  # run privreg
  i <- 1L
  bhat_a[, i] <- as.vector(glm.fit(Xa, y, family = family,
                                   intercept = FALSE)$coefficients)
  eta_a[,  i] <- Xa %*% bhat_a[, i]
  bhat_b[, i] <- as.vector(glm.fit(Xb, y, family = family, offset = eta_a[, i],
                                   intercept = FALSE)$coefficients)
  eta_b[,  i] <- Xb %*% bhat_b[, i]

  while (!conv && i < maxit) {
    i <- i + 1L
    eta <- eta_a[, i - 1] + eta_b[, i - 1]
    dev <- sum(family$dev.resids(y, family$linkinv(eta), rep(1, N)))
    if (debug) cat("Local | Iteration:", i, "| deviance:",
                   format(dev, nsmall = 8), "\n")

    bhat_a[, i] <- as.vector(glm.fit(Xa, y, family = family,
                                     offset = eta_b[, i - 1],
                                     intercept = FALSE)$coefficients)
    eta_a[,  i] <- Xa %*% bhat_a[, i]
    bhat_b[, i] <- as.vector(glm.fit(Xb, y, family = family, offset = eta_a[, i],
                                     intercept = FALSE)$coefficients)
    eta_b[,  i] <- Xb %*% bhat_b[, i]

    diffs_a <- abs(bhat_a[, i] - bhat_a[, i - 1])
    diffs_b <- abs(bhat_b[, i] - bhat_b[, i - 1])
    if (sum(diffs_a) < tol || sum(diffs_b) < tol) conv <- TRUE
  }
  if (!conv) warning("Not converged!")

  # compute SE
  if (se) {
    if (debug) cat("Local | Done. Computing SE. \n")
    idx   <- seq(1, i, length.out = min(i, N))
    Eta_a <- eta_a[, idx]
    Eta_b <- eta_b[, idx]
    Eta   <- Eta_a + Eta_b
    Y     <- matrix(y, N, min(i, N))
    Mu    <- apply(Eta, 2, family$linkinv)
    Delta <- apply(Eta, 2, family$mu.eta)

    eta   <- eta_a[, i] + eta_b[, i]
    mu    <- family$linkinv(eta)
    delta <- family$mu.eta(eta)
    vari  <- family$variance(mu)
    w     <- sqrt(delta^2 / vari)

    Eps_a <- Eta_b + (Y - Mu) / Delta
    Eps_b <- Eta_a + (Y - Mu) / Delta

    Hhat_a <- (Eta_b) %*% MASS::ginv(Eps_a)
    Hhat_b <- (Eta_a) %*% MASS::ginv(Eps_b)

    eig_a  <- RSpectra::eigs(Hhat_a, k = Pb)
    eig_b  <- RSpectra::eigs(Hhat_b, k = Pa)

    Z_a    <- cbind(Xa, eig_a$vectors)
    Z_b    <- cbind(Xb, eig_b$vectors)

    covmat_unscaled_a <- solve(crossprod(Z_a*w))[1:Pa, 1:Pa]
    covmat_unscaled_b <- solve(crossprod(Z_b*w))[1:Pb, 1:Pb]

    df_r <- N - Pa - Pb
    if (family$family %in% c("poisson", "binomial")) {
      dispersion <- 1
    } else if (df_r > 0) {
      dispersion <- c(crossprod((y - mu) / delta)) / df_r
    } else {
      dispersion <- NaN
    }
    covmat_a <- dispersion * covmat_unscaled_a
    covmat_b <- dispersion * covmat_unscaled_b

    se_a <- Re(sqrt(diag(covmat_a)))
    se_b <- Re(sqrt(diag(covmat_b)))
  } else {
    se_a <- NULL
    se_b <- NULL
  }

  fit <- glm(y ~ Xa + Xb + 0, family = family)
  return(list(
    full = list(coef = unname(coef(fit)), se = unname(sqrt(diag(vcov(fit))))),
    priv = list(coef = c(bhat_a[,i], bhat_b[,i]), se = c(se_a, se_b), iter = i)
  ))
}

