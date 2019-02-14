context("test-privreg")

# automated testing is not really working :(

# ensure no servers running
httpuv::stopAllServers()

# create test data
set.seed(45)
S <- rWishart(1, 10, diag(10))[,,1] / 10
X <- MASS::mvrnorm(100, rep(0, 10), S)
b <- runif(10, -1, 1)
y <- X %*% b + rnorm(100, sd = sqrt(b %*% S %*% b))

alice <- privreg$new(X[, 1:5], y,  name = "alice", verbose = TRUE)
bob   <- privreg$new(X[, 6:10], y, name = "bob  ", verbose = TRUE)
alice$listen()
bob$connect("127.0.0.1")

# expect(bob$.__enclos_env__$private$ws$readyState() == 1, "bob not connected")
# expect(alice$.__enclos_env__$private$ws$request$HEADERS["connection"] ==
#          "Upgrade", "alice not connected")

alice$max_iter <- 300

alice$verbose <- FALSE
bob$verbose <- FALSE

alice$start()

Sys.sleep(10)

cbind(true =  coef(lm(y ~ X + 0)), priv = c(alice$beta, bob$beta))

