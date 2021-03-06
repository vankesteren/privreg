% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/privreg.R
\name{PrivReg}
\alias{PrivReg}
\title{Private regression with vertically partitioned data}
\value{
an R6 object of class PrivReg
}
\description{
Perform privacy-preserving regression modeling across different institutions.
This class implements regression with gaussian and binomial responses using
block coordinate descent.
}
\section{Usage}{
\preformatted{alice <- PrivReg$new(
  formula,
  data,
  family    = "gaussian",
  name      = "alice",
  verbose   = FALSE,
  debug     = FALSE,
  crypt_key = "testkey"
)

alice$listen()
alice$connect(127.0.0.1)
alice$disconnect()

alice$estimate()
alice$calculate_se()

alice$summary()
alice$coef()
alice$converged()
alice$plot_paths()
alice$elapsed()
}
}

\section{Arguments}{

\itemize{
\item \code{formula} model formula for the regression model at this institution
\item \code{data} data frame for the variables in the model formula
\item \code{family} response family as in glm. Currently only gaussian and binomial are supported
\item \code{intercept} whether to include the intercept. Always use this instead of \code{+ 0} in the model formula
\item \code{name} name of this institution
\item \code{verbose} whether to print information
\item \code{debug} whether to print debug statements
\item \code{crypt_key} pre-shared key used to encrypt communication
}
}

\section{Details}{

\itemize{
\item \code{$new()} instantiates and returns a new PrivReg object.
\item \code{$listen()} listens for incoming connections from a partner institution
\item \code{$connect()} connects to a listening partner institution
\item \code{$disconnect()} disconnects from the partner institution
\item \code{$set_control()} sets control parameters. See below for more info
\item \code{$estimate()} computes parameter estimates through block coordinate descent
\item \code{$calculate_se()} computes standard errors using projection method
\item \code{$converged()} test whether the algorithm has converged
\item \code{$summary()} displays a summary of the object, invisibly returns the coef matrix
\item \code{$coef()} returns the model coefficients
\item \code{$plot_paths()} plots the paths of the parameters over the estimation iterations
\item \code{$elapsed()} print information about the elapsed time
}
}

\section{Control parameters}{

\itemize{
\item \code{max_iter} maximum number of iterations of the coordinate descent algorithm
\item \code{tol} PrivReg is converged if all beta changes are below \code{tol}.
\item \code{se} Whether to compute standard errors
}
}

\examples{
\dontrun{
# generate some data
set.seed(45)
X <- matrix(rnorm(1000), 100)
b <- runif(10, -1, 1)
y <- X \%*\% b + rnorm(100, sd = sqrt(b \%*\% S \%*\% b))

# split into alice and bob institutions
alice_data <- data.frame(y, X[, 1:5])
bob_data   <- data.frame(y, X[, 6:10])

# create connection
alice$listen()
bob$connect("127.0.0.1") # if alice is on different computer, change ip

# estimate
alice$estimate()

# ...

# compare results to lm()
summary(lm(y ~ X + 0))
alice$summary()
bob$summary()
}

}
