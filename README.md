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

## installation
```r
remotes::install_github("vankesteren/privreg")
```

## usage

```r
library(privreg)

# create test data
set.seed(45)
S <- cov2cor(rWishart(1, 10, diag(10))[,,1])
X <- cbind(MASS::mvrnorm(1000, rep(0, 10), S), rbinom(100, 1, 0.1))
b <- runif(11, -1, 1)
y <- X %*% b + rnorm(100, sd = sd(X %*% b))

# vertically partition test data
alice_data <- data.frame(y, X[, 1:5])
bob_data   <- data.frame(y, X[, 6:11])

# create alice and bob locations
alice <- PrivReg$new(
  formula   = y ~ .,
  data      = alice_data,
  family    = gaussian(),
  intercept = TRUE,
  verbose   = TRUE,
  name      = "alice",
  crypt_key = "pre-shared-key-123"
)

bob <- PrivReg$new(
  formula   = y ~ .,
  data      = bob_data,
  family    = gaussian(),
  intercept = FALSE,
  verbose   = TRUE,
  name      = "bob  ",
  crypt_key = "pre-shared-key-123"
)

# connect the session
alice$listen()
bob$connect(url = "127.0.0.1")

# ...

# estimate the model
alice$estimate()

# ...

# disconnect
alice$disconnect()

# compare results to glm()
summary(glm(y ~ X))
alice$summary()
bob$summary()
```

```
> summary(glm(y ~ X))

Call:
glm(formula = y ~ X)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-2.17875  -0.73828  -0.05085   0.72709   2.45231  

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.10280    0.03419  -3.007 0.002708 ** 
X1           0.86003    0.19318   4.452 9.48e-06 ***
X2          -0.26996    0.12100  -2.231 0.025903 *  
X3           0.85155    0.10760   7.914 6.65e-15 ***
X4          -1.07249    0.13248  -8.096 1.67e-15 ***
X5           0.36944    0.11597   3.186 0.001490 ** 
X6           1.05666    0.16451   6.423 2.07e-10 ***
X7           0.37315    0.11277   3.309 0.000971 ***
X8          -0.61272    0.19582  -3.129 0.001806 ** 
X9           0.44891    0.10730   4.184 3.12e-05 ***
X10         -0.69175    0.18520  -3.735 0.000198 ***
X11         -0.65930    0.12914  -5.105 3.96e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 1.080014)

    Null deviance: 2142.7  on 999  degrees of freedom
Residual deviance: 1067.1  on 988  degrees of freedom
AIC: 2928.8

Number of Fisher Scoring iterations: 2

> alice$summary()

Privacy-preserving GLM
----------------------

family:      gaussian
formula:     y ~ .
iterations:  399

Coefficients:
             Estimate Std. Error     2.5%     97.5% t value  Pr(>|t|)    
(Intercept) -0.102797   0.034189 -0.16989 -0.035705 -3.0067  0.002708 ** 
X1           0.860031   0.193180  0.48094  1.239122  4.4520 9.478e-06 ***
X2          -0.269965   0.121005 -0.50742 -0.032509 -2.2310  0.025903 *  
X3           0.851555   0.107599  0.64041  1.062703  7.9142 6.653e-15 ***
X4          -1.072488   0.132478 -1.33246 -0.812517 -8.0956 1.666e-15 ***
X5           0.369441   0.115970  0.14186  0.597018  3.1857  0.001490 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

> bob$summary()

Privacy-preserving GLM
----------------------

family:      gaussian
formula:     y ~ .
iterations:  400

Coefficients:
   Estimate Std. Error     2.5%    97.5% t value  Pr(>|t|)    
X1  1.05666    0.16451  0.73382  1.37949  6.4229 2.074e-10 ***
X2  0.37315    0.11277  0.15185  0.59444  3.3089 0.0009705 ***
X3 -0.61272    0.19582 -0.99700 -0.22844 -3.1289 0.0018059 ** 
X4  0.44891    0.10730  0.23835  0.65947  4.1837 3.123e-05 ***
X5 -0.69175    0.18520 -1.05519 -0.32832 -3.7351 0.0001984 ***
X6 -0.65930    0.12914 -0.91272 -0.40588 -5.1053 3.961e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
