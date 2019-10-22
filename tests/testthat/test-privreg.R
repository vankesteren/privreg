context("Test PrivReg manually")
skip("Automated testing does not work yet.")

# automated testing is not really working :(

# ensure no servers running
httpuv::stopAllServers()

# create test data
set.seed(45)
S <- rWishart(1, 10, diag(10))[,,1] / 10
X <- cbind(MASS::mvrnorm(1000, rep(0, 10), S), rbinom(100, 1, 0.1))
b <- runif(11, -1, 1)
y <- X %*% b + rnorm(100, sd = sd(X %*% b))

alice_data <- data.frame(y, X[, 1:5])
bob_data   <- data.frame(y, X[, 6:11])

alice <- PrivReg$new(
  formula = y ~ .,
  data = alice_data,
  intercept = FALSE,
  name = "alice",
  verbose = TRUE,
  crypt_key = "maastricht"
)

bob <- PrivReg$new(
  formula = y ~ .,
  data = bob_data,
  intercept = FALSE,
  name = "bob  ",
  verbose = TRUE,
  crypt_key = "maastricht"
)

alice$listen()
bob$connect("127.0.0.1")

# do the thing
alice$estimate()

# compare results to lm()
summary(lm(y ~ X + 0))
alice$summary()
bob$summary()

# disconnect
alice$disconnect()



# ensure no servers running
httpuv::stopAllServers()

# binomial outcome
invlogit <- function(x) 1 / (1 + exp(-x))
set.seed(48)
S <- rWishart(1, 20, diag(20))[,,1] / 20
X <- MASS::mvrnorm(1000, rep(0, 20), S)
b <- runif(20, -1, 1)
y_binom  <- vapply(invlogit(X %*% b), function(p) rbinom(1, 1, prob = p), 1)

alice_data <- data.frame(y = y_binom, X[, 1:10])
bob_data   <- data.frame(y = y_binom, X[, 11:20])

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

alice$set_control(max_iter = 3e4, se = FALSE)
bob$set_control(max_iter = 3e4, se = FALSE)

# do the thing
alice$estimate()

# compare results to glm()
summary(glm(y_binom ~ X + 0, family = "binomial"))
alice$summary()
bob$summary()

# disconnect
alice$disconnect()



# ensure no servers running
httpuv::stopAllServers()

# poisson outcome
fam <- poisson()
set.seed(45)
S <- cov2cor(rWishart(1, 8, diag(8))[,,1])
X <- MASS::mvrnorm(1000, rep(0, 8), S)
b <- runif(8, -1, 1)
y_pois <- vapply(fam$linkinv(X %*% b), function(mu) rpois(1, mu), 1)


alice_data <- data.frame(y = y_pois, X[, 1:4])
bob_data   <- data.frame(y = y_pois, X[, 5:8])


alice <- PrivReg$new(
  formula = y ~ . + 0,
  data = alice_data,
  family = "poisson",
  name = "alice",
  verbose = TRUE,
  crypt_key = "maastricht"
)

bob <- PrivReg$new(
  formula = y ~ . + 0,
  data = bob_data,
  family = "poisson",
  name = "bob  ",
  verbose = TRUE,
  crypt_key = "maastricht"
)


alice$listen()
bob$connect("127.0.0.1")


# do the thing
alice$estimate()

# compare results to glm()
summary(glm(y_pois ~ X + 0, family = "poisson"))
alice$summary()
bob$summary()

# disconnect
alice$disconnect()
