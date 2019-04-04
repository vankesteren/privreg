<p align="center">
  <img src="logo.png" width="300px"></img>
  <br/>
  <span>
    <a href="https://CRAN.R-project.org/package=privreg"><img src="http://www.r-pkg.org/badges/version/privreg"></img></a>
    <a href="https://travis-ci.org/vankesteren/privreg"><img src="https://travis-ci.org/vankesteren/privreg.svg?branch=master"></img></a>
  </span>
  <h5 align="center">Private regression using block coordinate descent</h5>
</p>
<br/>

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


# create vertically partitioned datasets
alice_data <- data.frame(y, X[, 1:5])
bob_data   <- data.frame(y, X[, 6:10])

# create objects - you would create these at separate institutions.
alice <- PrivReg$new(y ~ . + 0, alice_data, name = "alice", verbose = TRUE,
                     crypt_key = "pre-shared-key123")
bob   <- PrivReg$new(y ~ . + 0, bob_data,   name = "bob  ", verbose = TRUE,
                     crypt_key = "pre-shared-key123")

# create connection
alice$listen()
bob$connect("127.0.0.1") # if alice is on different computer, change ip

# estimate
alice$start()

# ...

# compare results to lm()
cbind(true = coef(lm(y ~ X + 0)), 
      priv = c(alice$beta, bob$beta))
```
