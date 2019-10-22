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
S <- cov2cor(rWishart(1, 10, diag(10))[,,1])
X <- cbind(MASS::mvrnorm(1000, rep(0, 10), S), rbinom(100, 1, 0.1))
b <- runif(11, -1, 1)
y <- X %*% b + rnorm(100, sd = sd(X %*% b))

# vertically partition test data
alice_data <- data.frame(y, X[, 1:5])
bob_data   <- data.frame(y, X[, 6:11])

# create alice and bob locations
alice <- PrivReg$new(
  formula = y ~ .,
  data = alice_data,
  intercept = FALSE,
  name = "alice",
  verbose = TRUE,
  crypt_key = "pre-shared-key-123"
)

bob <- PrivReg$new(
  formula = y ~ .,
  data = bob_data,
  intercept = FALSE,
  name = "bob  ",
  verbose = TRUE,
  crypt_key = "pre-shared-key-123"
)

# connect the session
alice$listen()
bob$connect("127.0.0.1")

# ...

# estimate the model
alice$estimate()

# ...

# disconnect
alice$disconnect()

# compare results to glm()
summary(glm(y ~ X + 0))
alice$summary()
bob$summary()
```

```
> summary(glm(y ~ X + 0))

Call:
glm(formula = y ~ X + 0)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.2777  -0.8311  -0.1362   0.6463   2.3477  

Coefficients:
    Estimate Std. Error t value Pr(>|t|)    
X1    0.8524     0.1939   4.395 1.23e-05 ***
X2   -0.2743     0.1215  -2.258 0.024156 *  
X3    0.8480     0.1080   7.850 1.08e-14 ***
X4   -1.0706     0.1330  -8.049 2.39e-15 ***
X5    0.3804     0.1164   3.269 0.001117 ** 
X6    1.0523     0.1652   6.371 2.88e-10 ***
X7    0.3729     0.1132   3.294 0.001024 ** 
X8   -0.6051     0.1966  -3.078 0.002142 ** 
X9    0.4482     0.1077   4.160 3.46e-05 ***
X10  -0.6823     0.1859  -3.670 0.000256 ***
X11  -0.7625     0.1250  -6.100 1.52e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 1.088795)

    Null deviance: 2175.7  on 1000  degrees of freedom
Residual deviance: 1076.8  on  989  degrees of freedom
AIC: 2935.9

Number of Fisher Scoring iterations: 2


> alice$summary()

Privacy-preserving GLM
----------------------

family:      gaussian
formula:     y ~ .
iterations:  398

Coefficients:
   Estimate Std. Error     2.5%     97.5% t value  Pr(>|t|)    
X1  0.85238    0.19395  0.47178  1.232975  4.3949 1.228e-05 ***
X2 -0.27433    0.12149 -0.51273 -0.035928 -2.2581  0.024156 *  
X3  0.84801    0.10803  0.63602  1.060004  7.8499 1.079e-14 ***
X4 -1.07058    0.13301 -1.33160 -0.809556 -8.0486 2.389e-15 ***
X5  0.38045    0.11638  0.15206  0.608831  3.2689  0.001117 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


> bob$summary()

Privacy-preserving GLM
----------------------

family:      gaussian
formula:     y ~ .
iterations:  399

Coefficients:
   Estimate Std. Error     2.5%    97.5% t value  Pr(>|t|)    
X1  1.05231    0.16517  0.72818  1.37644  6.3709 2.875e-10 ***
X2  0.37294    0.11323  0.15074  0.59514  3.2937 0.0010239 ** 
X3 -0.60511    0.19660 -0.99092 -0.21931 -3.0779 0.0021424 ** 
X4  0.44819    0.10773  0.23678  0.65960  4.1602 3.456e-05 ***
X5 -0.68227    0.18593 -1.04713 -0.31741 -3.6696 0.0002559 ***
X6 -0.76248    0.12500 -1.00778 -0.51719 -6.0998 1.521e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
