#' Private regression with vertically partitioned data
#'
#' Perform privacy-preserving regression modeling across different institutions.
#' This class implements regression with gaussian and binomial responses using
#' block coordinate descent (Van Kesteren, Sun, and Ippel; in preparation).
#'
#' @name PrivReg
#'
#' @section Usage:
#' \preformatted{alice <- PrivReg$new(formula, data, family = "gaussian",
#'                      name = "alice", verbose = FALSE, crypt_key = "testkey")
#'
#' alice$listen()
#' alice$connect(127.0.0.1)
#' alice$disconnect()
#'
#' alice$estimate()
#' alice$profile()
#'
#' alice$loglik()
#' alice$summary()
#' alice$plot_paths()
#' alice$elapsed()
#' }
#'
#' @section Arguments:
#' \describe{
#'  \item{alice}{a PrivReg object}
#'  \item{formula}{model formula for the regression model at this institution}
#'  \item{data}{data frame for the variables in the model formula}
#'  \item{family}{response family as in glm. Currently only gaussian and binomial are supported}
#'  \item{name}{name of this institution}
#'  \item{verbose}{whether to print debug information}
#'  \item{crypt_key}{pre-shared key used to encrypt communication}
#' }
#'
#' @section Details:
#' \code{$new()} instantiates and returns a new PrivReg object.
#' \code{$listen()} listens for incoming connections from a partner institution
#' \code{$connect()} connects to a listening partner institution
#' \code{$disconnect()} disconnects from the partner institution
#' \code{$estimate()} computes parameter estimates through block coordinate descent
#' \code{$profile()} computes confidence intervals and standard errors using profile likelihood
#' \code{$loglik()} returns full model log-likelihood
#' \code{$summary()} displays a summary of the object
#' \code{$plot_paths()} plots the paths of the parameters over the estimation iterations
#' \code{$elapsed()} print information about the elapsed time in estimation and profiling
#'
#' @importFrom R6 R6Class
#'
#' @return an R6 object of class PrivReg
#'
#' @examples
#' \dontrun{
#' # generate some data
#' set.seed(45)
#' X <- matrix(rnorm(1000), 100)
#' b <- runif(10, -1, 1)
#' y <- X %*% b + rnorm(100, sd = sqrt(b %*% S %*% b))
#' # split into alice and bob institutions
#' alice_data <- data.frame(y, X[, 1:5])
#' bob_data   <- data.frame(y, X[, 6:10])
#' # create connection
#' alice$listen()
#' bob$connect("127.0.0.1") # if alice is on different computer, change ip
#' # estimate
#' alice$estimate()
#' # ...
#' # profile
#' alice$profile()
#' # ...
#' # compare results to lm()
#' summary(lm(y ~ X + 0))
#' alice$summary()
#' bob$summary()
#' }
#'
NULL

#' @export


PrivReg <- R6Class(
  classname = "PrivReg",
  public = list(
    beta          = NULL,
    family        = "gaussian",
    formula       = NULL,
    control       = list(
      iter      = 0L,
      prof_iter = 0L,
      max_iter  = 1e3L,
      tol       = 1e-8,
      prof_tol  = 1e-7,
      prof_Q    = 2
    ),
    verbose       = NULL,
    name          = NULL,
    crypt_key     = NULL,
    timings       = list(
      estimate = list(start = NULL, end = NULL),
      profile  = list(start = NULL, end = NULL)
    ),
    callback      = NULL,
    initialize    = function(formula, data, family = "gaussian", name = "alice",
                             verbose = FALSE, crypt_key = "testkey") {
      # init callback
      self$callback <- function() invisible(NULL)

      # public slots
      self$name      <- name
      self$verbose   <- verbose
      self$crypt_key <- crypt_key
      self$family    <- family
      self$formula   <- formula

      # private slots
      private$X             <- model.matrix(formula, data)
      private$y             <- unname(model.response(model.frame(formula, data)))
      private$N             <- nrow(private$X)
      private$P             <- ncol(private$X)
      private$betas         <- matrix(0, nrow = self$control$max_iter,
                                      ncol = private$P)
      private$pred_incoming <- rep(0, private$N)
      private$pred_outgoing <- rep(0, private$N)
    },
    set_control   = function(iter = 0L, prof_iter = 0L, max_iter = 1e3L,
                             tol = 1e-8, prof_tol = 1e-7) {
      self$control <- list(
        iter      = iter,
        prof_iter = prof_iter,
        max_iter  = max_iter,
        tol       = tol,
        prof_tol  = prof_tol
      )

      if (nrow(private$betas) > self$control$max_iter)
        private$betas <- private$betas[1:self$control$max_iter]
      if (nrow(private$betas) < self$control$max_iter)
        private$betas <- rbind(
          private$betas,
          matrix(0, nrow = self$control$max_iter - nrow(private$betas),
                 ncol = private$P)
        )
    },
    listen        = function(port = 8080, callback) {
      if (!missing(callback)) {
        if (!is.function(callback)) stop("Callback should be a function!")
        self$callback <- callback
      }
      if (self$verbose) cat(paste(self$name, "| starting server on port:", port, "\n"))
      private$srv <- httpuv::startServer(
        host = "0.0.0.0",
        port = port,
        app  = list(
          onHeaders = function(req) {
            # do nothing
          },
          onWSOpen = function(ws) {
            cat(self$name, "| Connection opened.\n")
            private$ws <- ws
            private$setup_ws_server()
          }
        )
      )
    },
    connect       = function(url, port = 8080, callback) {
      if (!missing(callback)) {
        if (!is.function(callback)) stop("Callback should be a function!")
        self$callback <- callback
      }
      if (self$verbose) cat(paste(self$name, "| opening websocket connection\n"))

      full_url <- paste0("ws://", url, ":", port)

      private$ws <- websocket::WebSocket$new(
        url = full_url,
        headers = list(Cookie = "private_regression"),
        accessLogChannels = "none",
        autoConnect = FALSE
      )
      private$setup_ws_client()
      private$ws$connect()
    },
    estimate      = function(callback) {
      if (!missing(callback)) {
        if (!is.function(callback)) stop("Callback should be a function!")
        self$callback <- callback
      }
      if (!self$connected()) stop("Connect to another institution first.")
      if (self$verbose) cat(paste(self$name, "| Performing initial run\n"))
      self$timings$estimate$start <- Sys.time()
      private$fit_model()
      self$control$iter <- self$control$iter + 1L
      private$compute_pred()
      private$send_pred(type = "estimate")
    },
    profile       = function(callback) {
      if (!missing(callback)) {
        if (!is.function(callback)) stop("Callback should be a function!")
        self$callback <- callback
      }
      if (self$verbose) cat(paste(self$name, "| Profiling...\n"))
      self$timings$profile$start <- Sys.time()
      private$get_marginal_ses()
      private$create_prof_probe()
      private$init_prof_pred_in()
      private$init_prof_pred_out()
      private$init_prof_betas()
      private$init_prof_conv()
      private$init_prof_lls()
      private$get_prof_betas()
      private$create_prof_preds()
      private$send_prof_preds()
    },
    disconnect    = function() {
      if (self$verbose) cat(paste(self$name, "| Disconnecting.\n"))
      if (inherits(private$ws, "WebSocket")) private$ws$close()
      if (!is.null(private$srv)) {
        httpuv::stopServer(private$srv)
        private$srv <- NULL
      }
    },
    connected     = function() {
      if (!is.null(private$srv)) {
        if (is.null(private$ws)) return(FALSE)
        status <- private$ws$request$HEADERS["connection"]
        if (status == "Upgrade") return(TRUE)
      } else if (!is.null(private$ws) && private$ws$readyState() == 1) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    converged     = function() {
      if (self$control$iter < 2) return(FALSE)
      diffs <- abs(private$betas[self$control$iter] - private$betas[self$control$iter - 1])
      if (all(diffs < self$control$tol)) TRUE else FALSE
    },
    plot_paths    = function() {
      if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2")
      if (!requireNamespace("dplyr", quietly = TRUE))   stop("Install dplyr")
      if (!requireNamespace("tidyr", quietly = TRUE))   stop("Install tidyr")
      if (!requireNamespace("tibble", quietly = TRUE))  stop("Install tibble")
      if (!requireNamespace("firatheme", quietly = TRUE))
        stop("Install firatheme: devtools::install_github(\"vankesteren/firatheme\")")
      `%>%` <- dplyr::`%>%`
      betas <- private$betas[1:self$control$iter,]
      colnames(betas) <- colnames(private$X)
      tibble::as_tibble(betas) %>%
        dplyr::mutate(iter = 1:self$control$iter) %>%
        tidyr::gather("param", "value", -iter) %>%
        ggplot2::ggplot(ggplot2::aes(x = iter, y = value, colour = param)) +
        ggplot2::geom_line(size = 1) +
        firatheme::theme_fira() +
        firatheme::scale_colour_fira() +
        ggplot2::labs(x = "Iteration", y = "Beta value", color = "Predictor")
    },
    loglik        = function() {
      private$get_loglik(self$beta, private$pred_incoming)
    },
    summary       = function() {
      frml <- as.character(self$formula)
      if (is.null(self$beta)) {
        tab <- NULL
      } else if (is.null(private$prof_ci)) {
        tab <- cbind(self$beta)
        rownames(tab) <- colnames(private$X)
        colnames(tab) <- "est"
      } else {
        # get the 95% ci values
        cis <- matrix(unlist(private$prof_ci), nrow = private$P, byrow = TRUE)
        ses <- apply(cis, 1, function(ci) {
          # compute the standard errors from the cis
          diff(ci)/2/qt(0.975, private$N)
        })
        tt <- self$beta / ses
        pp <- pt(-abs(tt), private$N - private$P) * 2

        tab <- cbind(self$beta, ses, tt, pp, cis)
        rownames(tab) <- colnames(private$X)
        colnames(tab) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)",
                           "2.5%", "97.5%")
      }
      cat(sep = "", "\n",
        "   Privacy-preserving GLM\n",
        "   ----------------------\n\n",
        "   family:      ", self$family, "\n",
        "   formula:     ", frml[2], " ", frml[1]," ", frml[3], "\n",
        "   iterations:  ", self$control$iter, "\n\n",
        "   Parameter estimates\n",
        "   -------------------\n\n"
      )
      print(round(tab, 4))
      invisible(tab)
    },
    elapsed       = function() {
      if (!is.null(self$timings$estimate$end)) {
        est_time <- self$timings$estimate$end - self$timings$estimate$start
        cat("Estimation took", format(est_time), "\n")
        units(est_time) <- "secs"

        if (!is.null(self$timings$profile$end)) {
          prof_time <- self$timings$profile$end - self$timings$profile$start
          cat("Profiling took", format(prof_time), "\n")
          units(prof_time) <- "secs"
        }
        invisible(data.frame(
          Estimation = est_time,
          Profile    = ifelse(is.null(self$timings$profile$end), NA, prof_time)
        ))
      } else {
        cat("No timing information.")
      }
    }
  ),
  private = list(
    # slots
    X               = NULL, # model matrix
    y               = NULL, # outcome var
    N               = NULL, # sample size
    P               = NULL, # number of preds

    betas           = NULL, # max_iter*P vector of parameters
    pred_incoming   = NULL, # N vector of the incoming predictions
    pred_outgoing   = NULL, # N vector of the outgoing predictions

    marginal_ses    = NULL, # P vector of marginal standard errors
    Q               = NULL, # Number of probes to test for prof lik
    prof_probes     = NULL, # P*Q matrix of profile likelihood probe points
    prof_corrects   = NULL, # N*P*Q array of correction factors
    prof_betas      = NULL, # P*P*Q array of current betas
    prof_pred_out   = NULL, # N*P*Q array of outgoing profile predictions
    prof_pred_in    = NULL, # N*P*Q array of incoming profile predictions
    prof_pred_ret   = NULL, # N*P*Q array of to return profile predictions
    prof_converged  = NULL, # P*Q matrix of converg per profile probe
    prof_lls        = NULL, # P*Q matrix profile lls belonging to probes
    prof_ll_coefs   = NULL, # list of profile ll quadratic coefs per parameter
    prof_ci         = NULL, # ci based on profile ll

    # callback
    run_callback    = function() {
      # reset the callback first and then run it
      cb <- self$callback
      self$callback <- function() invisible(NULL)
      do.call(cb, args = list())
    },

    # estimation
    run_estimate    = function() {
      if (is.null(self$timings$estimate$start))
        self$timings$estimate$start <- Sys.time()
      private$pred_incoming <- private$msg_incoming$data
      private$fit_model()
      private$compute_pred()
      self$control$iter <- self$control$iter + 1L
      if (self$verbose) cat(self$name, "| iteration:", self$control$iter, "\n")
      if (self$converged()) {
        message("PrivReg converged")
        self$timings$estimate$end <- Sys.time()
        private$send_pred(type = "final_iter")
        private$run_callback()
      } else if (self$control$iter < self$control$max_iter) {
        private$send_pred(type = "estimate")
      } else {
        message("Maximum iterations reached")
        self$timings$estimate$end <- Sys.time()
        private$send_pred(type = "final_iter")
        private$run_callback()
      }
    },
    final_estimate  = function() {
      private$pred_incoming <- private$msg_incoming$data
      private$fit_model()
      self$control$iter <- self$control$iter + 1L
      private$compute_pred()
      self$timings$estimate$end <- Sys.time()
      if (self$control$iter == self$control$max_iter) {
        message("Maximum iterations reached")
        private$run_callback()
      } else {
        message("Partner converged")
        private$fit_model()
        self$control$iter <- self$control$iter + 1L
        private$run_callback()
      }
    },
    fit_model       = function() {
      if (self$verbose) cat(paste(self$name, "| Computing beta.\n"))
      self$beta  <- switch(self$family,
        gaussian = fit_gaussian(private$y, private$X, private$pred_incoming),
        binomial = fit_binomial(private$y, private$X, private$pred_incoming,
                                private$pred_outgoing)
      )
      private$betas[self$control$iter + 1, ] <- self$beta
    },
    compute_pred    = function() {
      if (self$verbose) cat(paste(self$name, "| Computing prediction.\n"))
      private$pred_outgoing <- private$X %*% self$beta
    },
    send_pred       = function(type) {
      if (self$verbose) cat(paste(self$name, "| Sending prediction.\n"))
      private$send_message(type = type, data = private$pred_outgoing)
    },

    # profiling
    get_loglik         = function(b, pred_other) {
      switch(self$family,
        gaussian = ll_gaussian(b, private$y, private$X, pred_other),
        binomial = ll_binomial(b, private$y, private$X, pred_other)
      )
    },
    get_marginal_ses   = function() {
      private$marginal_ses <- switch(self$family,
        gaussian = {
          pred <- private$pred_incoming + private$pred_outgoing
          ssr  <- c(crossprod(private$y - pred))
          sig2 <- ssr / private$N
          sqrt(diag(sig2*solve(crossprod(private$X))))
        },
        binomial = {
          pred <- private$pred_incoming + private$pred_outgoing
          prob <- 1 / (1 + exp(-pred))
          wght <- prob * (1 - prob)
          W    <- Matrix::sparseMatrix(i = 1:length(wght), j = 1:length(wght),
                                       x = c(wght))
          sqrt(Matrix::diag(Matrix::solve(Matrix::crossprod(X, W %*% X))))
        }
      )
    },
    create_prof_probe  = function() {
      points <- switch(self$family,
        gaussian = seq(-1.5, 1, length.out = self$control$prof_Q),
        binomial = seq(-.4, .5, length.out = self$control$prof_Q)
      )
      private$Q    <- length(points)
      private$prof_probes <- matrix(0, private$P, private$Q)
      for (p in 1:private$P) {
        private$prof_probes[p, ] <-
          self$beta[p] + points * private$marginal_ses[p]
      }
      private$prof_corrects <- array(0, c(private$N, private$P, private$Q))
      for (p in 1:private$P) {
        for (q in 1:private$Q) {
          private$prof_corrects[ , p, q] <-
            private$X[,p] * private$prof_probes[p, q]
        }
      }
    },
    init_prof_pred_in  = function() {
      private$prof_pred_in <-
        array(private$pred_incoming, c(private$N, private$P, private$Q))
    },
    init_prof_pred_out = function() {
      private$prof_pred_out <-
        array(private$pred_outgoing, c(private$N, private$P, private$Q))
    },
    init_prof_betas    = function() {
      private$prof_betas <- array(self$beta, c(private$P, private$P, private$Q))
      for (p in 1:private$P) {
        for (q in 1:private$Q) {
          private$prof_betas[p, p, q] <- private$prof_probes[p, q]
        }
      }
    },
    init_prof_conv     = function() {
      private$prof_converged <- matrix(FALSE, private$P, private$Q)
    },
    init_prof_lls      = function() {
      private$prof_lls <- matrix(0, private$P, private$Q)
    },
    get_prof_betas     = function() {
      if (private$P == 1) return()
      for (p in 1:private$P) {
        for (q in 1:private$Q) {
          y_pred <- private$prof_corrects[,p,q] + private$prof_pred_in[,p,q]
          private$prof_betas[-p, p, q] <- switch(self$family,
            gaussian = fit_gaussian(
              y = private$y, X = private$X[,-p],pred_other = y_pred
            ),
            binomial = fit_binomial(
              y = private$y,
              X = private$X[,-p],
              pred_self = private$prof_pred_out[,p,q] -
                private$prof_corrects[,p,q],
              pred_other = y_pred
            )
          )
        }
      }
    },
    create_prof_preds  = function() {
      if (self$verbose) cat(paste(self$name, "| Making profile predictions.\n"))
      for (p in 1:private$P) {
        for (q in 1:private$Q) {
          private$prof_pred_out[,p,q] <- private$X %*% private$prof_betas[,p,q]
        }
      }
    },
    send_prof_preds    = function() {
      if (self$verbose) cat(paste(self$name, "| Sending profile predictions.\n"))
      private$send_message(type = "prof_pred", data = private$prof_pred_out)
    },
    return_prof_preds  = function() {
      prof_pred_in  <- private$msg_incoming$data
      pred_dims      <- dim(prof_pred_in)
      if (is.null(private$prof_pred_ret))
        private$prof_pred_ret <- array(private$pred_outgoing, pred_dims)

      for (p in 1:pred_dims[2]) {
        for (q in 1:pred_dims[3]) {
          pred_in <- prof_pred_in[,p,q]
          betas <- switch(self$family,
            gaussian = fit_gaussian(private$y, private$X, pred_in),
            binomial = fit_binomial(private$y, private$X, pred_in,
                                    private$prof_pred_ret[,p,q])
          )
          private$prof_pred_ret[,p,q] <- private$X %*% betas
        }
      }

      private$send_message(
        type = "return_prof_pred",
        data = private$prof_pred_ret
      )
    },
    receive_prof_pred  = function() {
      self$control$prof_iter <- self$control$prof_iter + 1L
      if (self$verbose)
        cat(self$name, "| iteration:", self$control$prof_iter,"\n")
      private$prof_pred_in <- private$msg_incoming$data
      private$get_prof_betas()
      private$compute_prof_lls()
      if (self$control$max_iter == self$control$prof_iter) {
        message("Maximum profile iteration reached.")
      } else if (all(private$prof_converged)) {
        message("Profiling converged.")
        private$finish_profile()
      } else {
        private$create_prof_preds()
        private$send_prof_preds()
      }
    },
    compute_prof_lls   = function() {
      for (p in 1:private$P) {
        for (q in 1:private$Q) {
          ll <- private$get_loglik(b = private$prof_betas[,p,q],
                                   pred_other = private$prof_pred_in[,p,q])
          if (abs(ll - private$prof_lls[p, q]) < self$control$prof_tol) {
            private$prof_converged[p, q] <- TRUE
          }
          private$prof_lls[p, q] <- ll
        }
      }

      private$send_message(
        type = "return_prof_pred",
        data = prof_preds_out
      )
    },
    receive_prof_pred = function() {
      self$control$prof_iter <- self$control$prof_iter + 1L
      if (self$verbose)
        cat(self$name, "| iteration:", self$control$prof_iter,"\n")
      private$prof_pred_in <- private$msg_incoming$data
      private$get_prof_betas()
      private$compute_prof_lls()
      if (self$control$max_iter == self$control$prof_iter) {
        message("Maximum profile iteration reached.")
      } else if (all(private$prof_converged)) {
        message("Profiling converged.")
        private$finish_profile()
      } else {
        private$create_prof_preds()
        private$send_prof_preds()
      }
    },
    finish_profile     = function() {
      private$compute_prof_coef()
      private$create_ci()
      self$timings$profile$end <- Sys.time()
      private$run_callback()
    },
    compute_prof_coef  = function() {
      ll_ml <- self$loglik()
      private$prof_ll_coefs <- vector("list", private$P)
      for (p in 1:private$P) {
        ll <- c(ll_ml, private$prof_lls[p,])
        bb <- c(self$beta[p], private$prof_probes[p,])
        private$prof_ll_coefs[[p]] <- unname(coef(stats::lm(ll ~ bb + I(bb^2))))
      }
    },
    create_ci          = function() {
      ml_llk <- self$loglik()
      private$prof_ci <- lapply(1:private$P, function(p) {
        # find the roots using the quadratic approximation of the pl function
        # ci fun = 2(pl(b) - ll(b_hat)) + qchisq(0.95, 1)
        A <- 2*private$prof_ll_coefs[[p]][3]
        B <- 2*private$prof_ll_coefs[[p]][2]
        C <- 2*(private$prof_ll_coefs[[p]][1] - ml_llk) + qchisq(0.95, 1)

        # never in my life did I think I'd use this formula again...
        root_1 <- (-B - sqrt(B^2 - 4*A*C)) / (2*A)
        root_2 <- (-B + sqrt(B^2 - 4*A*C)) / (2*A)

        lower <- min(c(root_1, root_2))
        upper <- max(c(root_1, root_2))

        c(lower, upper)
      })
    },

    # networking
    ws              = NULL, # the websocket object
    srv             = NULL, # the httpuv server object
    setup_ws_server = function() {
      # httpuv websocket has different api than websocket :(
      # see https://github.com/rstudio/httpuv/blob/12744e32b0ef8480d8cffb12e9888cbae7f12778/R/httpuv.R#L291
      if (self$verbose) cat(paste(self$name, "| setting up ws.\n"))

      private$ws$onMessage(function(binary, message) {
        if (!binary) {
          # something went wrong
          private$disconnect()
          stop("Message received was not binary.")
        }
        private$msg_enc_in <- message
        private$receive_message()
      })

      private$ws$onClose(function() {
        cat(self$name, "| Connection closed.\n")
      })

      private$run_callback()
    },
    setup_ws_client = function() {
      # websocket has different api than httuv websocket :(
      if (self$verbose) cat(paste(self$name, "| setting up ws.\n"))

      private$ws$onOpen(function(event) {
        cat(self$name, "| Connection opened.\n")
        private$run_callback()
      })

      private$ws$onMessage(function(event) {
        private$msg_enc_in <- event$data
        private$receive_message()
      })

      private$ws$onClose(function(event) {
        cat(self$name, "| Connection closed.\n")
      })
    },

    # communication
    msg_incoming    = NULL, # the decrypted incoming message
    msg_enc_in      = NULL, # the encrypted incoming message
    msg_chunks_in   = list(), # bits for chunked messages
    data_encrypt    = function(dat, key) {
      # converts numeric vector to secure binary message
      serialized <- serialize(dat, NULL)
      key_bytes  <- charToRaw(openssl::md5(key))
      return(openssl::aes_cbc_encrypt(serialized, key_bytes, NULL))
    },
    data_decrypt    = function(raw, key) {
      key_bytes <- charToRaw(openssl::md5(key))
      unserialize(openssl::aes_cbc_decrypt(raw, key_bytes, NULL))
    },
    send_message    = function(type, ...) {
      msg_raw <- list(type = type, ...)
      msg_enc <- private$data_encrypt(msg_raw, self$crypt_key)

      msg_len <- length(msg_enc)
      if (msg_len < 31457280) { # 30 MB maximum for websocket connection
        private$ws$send(msg_enc)
      } else {
        # now we need to chunk!
        cuts    <- c(seq(0, msg_len, 31457280), msg_len) # 30MB chunks
        cut_len <- length(cuts)
        if (self$verbose)
          cat(paste(self$name, "| Transmit data > 30MB, chunking ... \n"))
        for (i in 2:cut_len) {
          bits <- msg_enc[(cuts[i - 1] + 1):cuts[i]]
          type <- ifelse(i == cut_len, "chunk_last", "chunk")
          part_msg_raw <- list(type = type, no = i - 1, bits = bits)
          part_msg_enc <- private$data_encrypt(part_msg_raw, self$crypt_key)
          private$ws$send(part_msg_enc)
          if (self$verbose)
            cat(paste(self$name, "|", i - 1, "/", cut_len - 1, "\n"))
        }
      }

    },
    receive_message = function() {
      private$msg_incoming <- private$data_decrypt(private$msg_enc_in, self$crypt_key)

      # chunking stuff
      if (private$msg_incoming$type == "chunk") {
        # message is chunked!
        no <- private$msg_incoming$no
        if (self$verbose)
          cat(paste(self$name, "| Receiving chunked message,", no, "\n"))
        private$msg_chunks_in[[no]] <- private$msg_incoming$bits
        return()
      }
      if (private$msg_incoming$type == "chunk_last") {
        no <- private$msg_incoming$no
        if (self$verbose)
          cat(paste(self$name, "| Receiving chunked message,", no, "(end)\n"))
        private$msg_enc_in <- c(unlist(private$msg_chunks_in),
                                private$msg_incoming$bits)
        private$msg_chunks_in <- NULL
        private$receive_message()
        return()
      }

      if (self$verbose)
        cat(paste(self$name, "|", private$msg_incoming$type, "\n"))

      switch(private$msg_incoming$type,
        "estimate"         = private$run_estimate(),
        "final_iter"       = private$final_estimate(),
        "prof_pred"        = private$return_prof_preds(),
        "return_prof_pred" = private$receive_prof_pred()
      )
    }
  )
)
