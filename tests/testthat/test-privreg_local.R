context("Test PrivReg local")

# Gaussian outcome
set.seed(45)
S <- cov2cor(rWishart(1, 10, diag(10))[,,1])
X <- cbind(MASS::mvrnorm(1000, rep(0, 10), S), rbinom(100, 1, 0.1))
b <- runif(11, -1, 1)
e <- rnorm(100, sd = sd(X %*% b))
y <- X %*% b + e

res <- privreg_local(y, X[,1:5], X[,6:10], debug = FALSE)

test_that("Gaussian coefficients are the same", {
  expect_equal(res$full$coef, res$priv$coef)
})

test_that("Gaussian standard errors are the same", {
  expect_equal(res$full$se, res$priv$se)
})


# Binomial outcome
fam <- binomial()
y   <- vapply(fam$linkinv(X %*% b + e), function(mu) rbinom(1, 1, prob = mu), 1)
res <- privreg_local(y, X[,1:5], X[,6:10], family = fam, debug = FALSE)

test_that("Binomial coefficients are the same", {
  expect_equal(res$full$coef, res$priv$coef)
})

test_that("Binomial standard errors are within tolerance", {
  expect(
    all(abs(res$full$se - res$priv$se) < 1e-5),
    failure_message = paste(
      "Standard error are not within tolerance.",
      "\nMean deviation:", mean(abs(res$full$se - res$priv$se))
    )
  )
})


# poisson outcome
fam <- poisson()
y   <- vapply(fam$linkinv(X %*% b + e), function(mu) rpois(1, mu), 1)
res <- privreg_local(y, X[,1:5], X[,6:10], family = fam, debug = FALSE)


test_that("Binomial coefficients are the same", {
  expect_equal(res$full$coef, res$priv$coef)
})

test_that("Binomial standard errors are within tolerance", {
  expect(
    all(abs(res$full$se - res$priv$se) < 1e-5),
    failure_message = paste(
      "Standard error are not within tolerance.",
      "\nMean deviation:", mean(abs(res$full$se - res$priv$se))
    )
  )
})
