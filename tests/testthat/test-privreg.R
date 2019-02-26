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

alice <- PrivReg$new(y ~ . + 0, data = alice_data, name = "alice", verbose = TRUE)
bob   <- PrivReg$new(y ~ . + 0, data = bob_data,   name = "bob  ", verbose = TRUE)
alice$listen()
bob$connect("127.0.0.1")

Sys.sleep(3)

expect(bob$connected(), "bob not connected")
expect(alice$connected(), "alice not connected")

alice$verbose <- FALSE
bob$verbose <- FALSE

alice$start()

Sys.sleep(10)

cbind(true =  coef(lm(y ~ X + 0)), priv = c(alice$beta, bob$beta))

