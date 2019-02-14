# `privreg`: Private regression using block coordinate descent


## install
```r
devtools::install_github("vankesteren/privreg")
```

## usage

```r
# create test data
set.seed(45)
S <- rWishart(1, 10, diag(10))[,,1] / 10
X <- MASS::mvrnorm(100, rep(0, 10), S)
b <- runif(10, -1, 1)
y <- X %*% b + rnorm(100, sd = sqrt(b %*% S %*% b))

# create objects with vertically partitioned datasets
# you would create these at separate institutions.
alice <- privreg$new(X[, 1:5],  y, name = "alice", verbose = TRUE)
bob   <- privreg$new(X[, 6:10], y, name = "bob  ", verbose = TRUE)

# create connection
alice$listen()
bob$connect("127.0.0.1") # if alice is on different computer, change ip

# estimate
alice$max_iter <- 300
alice$start()

# ...

# compare results to lm()
cbind(true =  coef(lm(y ~ X + 0)), 
      priv = c(alice$beta, bob$beta))
```
