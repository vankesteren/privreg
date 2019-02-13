
#
#
# X <- MASS::mvrnorm(100, rep(0, 10), rWishart(1, 10, diag(10))[,,1])
# b_true <- runif(10, -1, 1)
#
# y <- X %*% b_true + rnorm(100, sd = sqrt(crossprod(b_true)))
#
# alice <- privreg$new(X[, 1:5], y,  name = "alice")
# bob   <- privreg$new(X[, 6:10], y, name = "bob  ")
#
# alice$listen()
# bob$connect("ws://127.0.0.1:8080")
#
# alice$start()
