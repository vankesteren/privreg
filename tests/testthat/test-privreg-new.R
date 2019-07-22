context("Test PrivReg")
#skip("Automated testing does not work yet.")

# automated testing is not really working :(

# ensure no servers running
httpuv::stopAllServers()

# create tests as functions
test_lm <- function() {
  set.seed(45)
  S <- rWishart(1, 10, diag(10))[,,1] / 10
  X <- MASS::mvrnorm(100, rep(0, 10), S)
  b <- runif(10, -1, 1)
  y <- X %*% b + rnorm(100, sd = sqrt(b %*% S %*% b))

  alice_data <- data.frame(y, X[, 1:5])
  bob_data   <- data.frame(y, X[, 6:10])

  alice <- PrivReg$new(
    formula   = y ~ . + 0,
    data      = alice_data,
    name      = "alice",
    verbose   = FALSE,
    crypt_key = "maastricht"
  )

  bob <- PrivReg$new(
    formula   = y ~ . + 0,
    data      = bob_data,
    name      = "bob  ",
    verbose   = FALSE,
    crypt_key = "maastricht"
  )


  lm_run <- function() {
    # beautiful callback hell
    alice$listen(callback = function()
      alice$estimate(callback = function()
        alice$bootstrap(callback = function() {
          alice$disconnect()
          do.call(check_lm, list())
          do.call(test_glm, list(), envir = parent.frame(2))
        })
      )
    )
    bob$connect("127.0.0.1")
  }

  check_lm <- function() {

    test_that("Linear models parameter estimates are correct", {
      lm_fit <- lm(y ~ X + 0)
      privreg_beta <- round(c(alice$beta, bob$beta), 4)
      linmodl_beta <- unname(round(coef(lm_fit), 4))
      expect_equal(privreg_beta, linmodl_beta)

      privreg_se <- unname(c(alice$summary()[,2], bob$summary()[,2]))
      linmodl_se <- unname(summary(lm_fit)$coefficients[,2])
      expect_true(all(abs(privreg_se - linmodl_se) < 0.1))
    })
  }

  lm_run()
}

test_glm <- function() {
  # binomial outcome
  invlogit <- function(x) 1 / (1 + exp(-x))
  set.seed(45)
  S <- rWishart(1, 10, diag(10))[,,1] / 10
  X <- MASS::mvrnorm(100, rep(0, 10), S)
  b <- runif(10, -1, 1)
  y_binom  <- vapply(invlogit(X %*% b), function(p) rbinom(1, 1, prob = p), 1)

  alice_data <- data.frame(y = y_binom, X[, 1:5])
  bob_data   <- data.frame(y = y_binom, X[, 6:10])

  alice <- PrivReg$new(
    formula = y ~ . + 0,
    data = alice_data,
    family = "binomial",
    name = "alice",
    verbose = FALSE,
    crypt_key = "maastricht"
  )

  bob <- PrivReg$new(
    formula = y ~ . + 0,
    data = bob_data,
    family = "binomial",
    name = "bob  ",
    verbose = FALSE,
    crypt_key = "maastricht"
  )

  glm_run <- function() {
    # beautiful callback hell
    alice$listen(callback = function()
      alice$estimate(callback = function()
        alice$bootstrap(callback = function() {
          alice$disconnect()
          do.call(check_glm, list())
        })
      )
    )
    bob$connect("127.0.0.1")
  }
  check_glm <- function() {
    test_that("Logistic models parameter estimates are correct", {
    glm_fit <- glm(y_binom ~ X + 0, family = "binomial")
    privreg_beta <- round(c(alice$beta, bob$beta), 4)
    linmodl_beta <- unname(round(coef(glm_fit), 4))
    expect_equal(privreg_beta, linmodl_beta)

    privreg_se <- unname(c(alice$summary()[,2], bob$summary()[,2]))
    linmodl_se <- unname(summary(lm_fit)$coefficients[,2])
    expect_true(all(abs(privreg_se - linmodl_se) < 0.1))
    })
  }

  glm_run()
}


# run tests
test_lm()
