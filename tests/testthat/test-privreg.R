context("Test PrivReg")
skip("Automated testing does not work yet.")

# automated testing is not really working :(

# ensure no servers running
httpuv::stopAllServers()

# create test data
set.seed(45)
S <- rWishart(1, 10, diag(10))[,,1] / 10
X <- MASS::mvrnorm(100, rep(0, 10), S)
b <- runif(10, -1, 1)
y <- X %*% b + rnorm(100, sd = sqrt(b %*% S %*% b))

alice_data <- data.frame(y, X[, 1:5])
bob_data   <- data.frame(y, X[, 6:10])

alice <- PrivReg$new(
  formula = y ~ . + 0,
  data = alice_data,
  name = "alice",
  verbose = TRUE,
  crypt_key = "maastricht"
)

bob <- PrivReg$new(
  formula = y ~ . + 0,
  data = bob_data,
  name = "bob  ",
  verbose = TRUE,
  crypt_key = "maastricht"
)

alice$listen()
bob$connect("127.0.0.1")


alice$verbose <- FALSE
bob$verbose <- TRUE

# do the thing
alice$estimate()
alice$bootstrap(R = 100)

# compare results to lm()
summary(lm(y ~ X + 0))
alice$summary()
bob$summary()



# ensure no servers running
httpuv::stopAllServers()

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
  verbose = TRUE,
  crypt_key = "maastricht"
)

bob <- PrivReg$new(
  formula = y ~ . + 0,
  data = bob_data,
  family = "binomial",
  name = "bob  ",
  verbose = TRUE,
  crypt_key = "maastricht"
)

alice$listen()
bob$connect("127.0.0.1")

# do the thing
alice$estimate()
alice$bootstrap(R = 100)

# compare results to glm()
summary(glm(y_binom ~ X + 0, family = "binomial"))
alice$summary()
bob$summary()
