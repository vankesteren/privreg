#' Private regression with vertically partitioned data
#'
#' Perform privacy-preserving regression modeling across different institutions.
#' This class implements regression with gaussian and binomial responses using
#' block coordinate descent.
#'
#' @name PrivReg
#'
#' @section Usage:
#' ```
#' alice <- PrivReg$new(
#'   formula,
#'   data,
#'   family    = "gaussian",
#'   name      = "alice",
#'   verbose   = FALSE,
#'   debug     = FALSE,
#'   crypt_key = "testkey"
#' )
#'
#' alice$listen()
#' alice$connect(127.0.0.1)
#' alice$disconnect()
#'
#' alice$estimate()
#' alice$calculate_se()
#'
#' alice$summary()
#' alice$coef()
#' alice$converged()
#' alice$plot_paths()
#' alice$elapsed()
#' ```
#'
#' @section Arguments:
#'  - `formula` model formula for the regression model at this institution
#'  - `data` data frame for the variables in the model formula
#'  - `family` response family as in glm. Currently only gaussian and binomial are supported
#'  - `intercept` whether to include the intercept. Always use this instead of `+ 0` in the model formula
#'  - `name` name of this institution
#'  - `verbose` whether to print information
#'  - `debug` whether to print debug statements
#'  - `crypt_key` pre-shared key used to encrypt communication
#'
#'
#' @section Details:
#'  - `$new()` instantiates and returns a new PrivReg object.
#'  - `$listen()` listens for incoming connections from a partner institution
#'  - `$connect()` connects to a listening partner institution
#'  - `$disconnect()` disconnects from the partner institution
#'  - `$set_control()` sets control parameters. See below for more info
#'  - `$estimate()` computes parameter estimates through block coordinate descent
#'  - `$calculate_se()` computes standard errors using projection method
#'  - `$converged()` test whether the algorithm has converged
#'  - `$summary()` displays a summary of the object, invisibly returns the coef matrix
#'  - `$coef()` returns the model coefficients
#'  - `$plot_paths()` plots the paths of the parameters over the estimation iterations
#'  - `$elapsed()` print information about the elapsed time
#'
#'
#'
#' @section Control parameters:
#'  - `max_iter` maximum number of iterations of the coordinate descent algorithm
#'  - `tol` PrivReg is converged if all beta changes are below `tol`.
#'  - `se` Whether to compute standard errors
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
#'
#' # split into alice and bob institutions
#' alice_data <- data.frame(y, X[, 1:5])
#' bob_data   <- data.frame(y, X[, 6:10])
#'
#' # create connection
#' alice$listen()
#' bob$connect("127.0.0.1") # if alice is on different computer, change ip
#'
#' # estimate
#' alice$estimate()
#'
#' # ...
#'
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
    beta         = NULL,
    SE           = NULL,
    family       = "gaussian",
    formula      = NULL,
    verbose      = NULL,
    debug        = NULL,
    name         = NULL,
    crypt_key    = NULL,
    timings      = list(
      estimate = list(start = NULL, end = NULL),
      se       = list(start = NULL, end = NULL)
    ),
    callback     = NULL,
    initialize   = function(formula, data, family = "gaussian",
                            intercept = TRUE, name = "alice", verbose = FALSE,
                            debug = FALSE, crypt_key = "testkey") {
      # init callback
      self$callback <- function() invisible(NULL)

      # public slots
      self$name      <- name
      self$verbose   <- verbose
      self$debug     <- debug
      self$crypt_key <- crypt_key
      self$formula   <- formula

      # family
      if (is.character(family))
        family <- get(family, mode = "function", envir = parent.frame())
      if (is.function(family))
        self$family <- family()
      if (is.null(self$family$family)) {
        print(self$family)
        stop("'family' not recognized")
      }

      # data information
      private$X     <- model.matrix(formula, data)
      if (!intercept && colnames(private$X)[1] == "(Intercept)")
        private$X <- private$X[, -1]
      private$y     <- unname(model.response(model.frame(formula, data)))
      private$N     <- nrow(private$X)
      private$P     <- ncol(private$X)

      # Compute marginal parameters
      self$beta <- stats::glm.fit(
        x      = private$X,
        y      = private$y,
        family = self$family
      )$coefficients

      # prepare for iterations
      private$betas <- matrix(self$beta, private$P, private$control$max_iter)
      private$pred_incoming <- matrix(0, private$N, private$control$max_iter)
      private$pred_outgoing <- matrix(0, private$N, private$control$max_iter)
    },
    set_control  = function(iter = 0L, max_iter = 1e4L, tol = 1e-8, se = TRUE) {
      old_max_iter <- private$control$max_iter
      private$control <- list(
        iter     = iter,
        max_iter = max_iter,
        tol      = tol,
        se       = se
      )
      if (old_max_iter >= max_iter) {
        private$betas         <- private$betas[, 1:max_iter]
        private$pred_incoming <- private$pred_incoming[, 1:max_iter]
        private$pred_outgoing <- private$pred_outgoing[, 1:max_iter]
      } else {
        add <- max_iter - old_max_iter
        private$betas <-
          cbind(private$betas, matrix(self$beta, nrow = private$P, ncol = add))
        private$pred_incoming <-
          cbind(private$pred_incoming, matrix(0, nrow = private$N, ncol = add))
        private$pred_outgoing <-
          cbind(private$pred_outgoing, matrix(0, nrow = private$N, ncol = add))
      }
    },
    listen       = function(port = 8080, callback) {
      if (!missing(callback)) {
        if (!is.function(callback)) stop("Callback should be a function!")
        self$callback <- callback
      }
      if (self$debug)
        cat(paste(self$name, "| starting server on port:", port, "\n"))
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
    connect      = function(url, port = 8080, callback) {
      if (!missing(callback)) {
        if (!is.function(callback)) stop("Callback should be a function!")
        self$callback <- callback
      }
      if (self$debug) cat(paste(self$name, "| opening websocket connection\n"))

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
    estimate     = function(callback) {
      if (!missing(callback)) {
        if (!is.function(callback)) stop("Callback should be a function!")
        self$callback <- callback
      }
      if (!self$connected()) stop("Connect to another institution first.")
      if (self$debug) cat(paste(self$name, "| Performing initial run\n"))
      self$timings$estimate$start <- Sys.time()
      private$control$iter <- private$control$iter + 1L
      private$fit_model()
      private$compute_pred()

      # transfer dimension info before transferring prediction
      private$send_p()
    },
    calculate_se = function(callback) {
      if (!missing(callback)) {
        if (!is.function(callback)) stop("Callback should be a function!")
        self$callback <- callback
      }
      private$compute_se()
      private$run_callback()
    },
    disconnect   = function() {
      if (self$debug) cat(paste(self$name, "| Disconnecting.\n"))
      if (inherits(private$ws, "WebSocket")) private$ws$close()
      if (!is.null(private$srv)) {
        httpuv::stopServer(private$srv)
        private$srv <- NULL
      }
    },
    connected    = function() {
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
    converged    = function() {
      i <- private$control$iter
      if (i < 2) return(FALSE)
      diffs <- abs(private$betas[, i] - private$betas[, i - 1])
      sum(diffs) < private$control$tol
    },
    deviance     = function() {
      i   <- private$control$iter
      eta <- private$pred_incoming[, i] + private$pred_outgoing[, i]
      mu  <- self$family$linkinv(eta)
      return(sum(self$family$dev.resids(private$y, mu, rep(1, private$N))))
    },
    residuals    = function() {
      i    <- private$control$iter
      eta  <- private$pred_incoming[, i] + private$pred_outgoing[, i]
      mu   <- self$family$linkinv(eta)
      return((private$y - mu)/self$family$mu.eta(eta))
    },
    plot_paths   = function() {
      if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2")
      if (!requireNamespace("dplyr", quietly = TRUE))   stop("Install dplyr")
      if (!requireNamespace("tidyr", quietly = TRUE))   stop("Install tidyr")
      if (!requireNamespace("tibble", quietly = TRUE))  stop("Install tibble")
      if (!requireNamespace("firatheme", quietly = TRUE))
        stop("Install firatheme: devtools::install_github(\"vankesteren/firatheme\")")
      `%>%` <- dplyr::`%>%`
      betas <- t(private$betas[, 1:private$control$iter])
      colnames(betas) <- colnames(private$X)
      tibble::as_tibble(betas) %>%
        dplyr::mutate(iter = 1:private$control$iter) %>%
        tidyr::gather("param", "value", -iter) %>%
        ggplot2::ggplot(ggplot2::aes(x = iter, y = value, colour = param)) +
        ggplot2::geom_line(size = 1) +
        firatheme::theme_fira() +
        firatheme::scale_colour_fira() +
        ggplot2::labs(x = "Iteration", y = "Beta value", color = "Predictor")
    },
    summary      = function() {
      frml <- as.character(self$formula)
      if (is.null(self$SE)) {
        cat(sep = "", "\n",
            "Privacy-preserving GLM\n",
            "----------------------\n\n",
            "family:      ", self$family$family, "\n",
            "formula:     ", frml[2], " ", frml[1]," ", frml[3], "\n",
            "iterations:  ", private$control$iter, "\n\n",
            "Coefficients:\n")
        tab <- as.matrix(self$beta)
        rownames(tab) <- colnames(private$X)
        colnames(tab) <- "Estimate"
        print(tab)
        return(invisible(tab))
      }

      bts <- self$beta
      ses <- self$SE
      cil <- bts - qt(0.975, private$N - private$P - private$Pp) * ses
      ciu <- bts + qt(0.975, private$N - private$P - private$Pp) * ses
      tt  <- bts / ses
      pp  <- pt(-abs(tt), private$N - private$P - private$Pp) * 2

      tab <- cbind(bts, ses, cil, ciu, tt, pp)
      rownames(tab) <- colnames(private$X)
      colnames(tab) <- c("Estimate", "Std. Error", "2.5%", "97.5%",
                         "t value", "Pr(>|t|)")

      cat(sep = "", "\n",
        "Privacy-preserving GLM\n",
        "----------------------\n\n",
        "family:      ", self$family$family, "\n",
        "formula:     ", frml[2], " ", frml[1]," ", frml[3], "\n",
        "iterations:  ", private$control$iter, "\n\n",
        "Coefficients:\n"
      )
      stats::printCoefmat(tab, cs.ind = c(1, 2), tst.ind = 5,
                          P.values = TRUE, has.Pvalue = TRUE)
      cat("\n")
      invisible(tab)
    },
    coef         = function() {
      coefs <- self$beta
      names(coefs) <- colnames(private$X)
      coefs
    },
    elapsed      = function() {
      if (!is.null(self$timings$estimate$end)) {
        est_time <- self$timings$estimate$end - self$timings$estimate$start
        cat("Estimation took", format(est_time), "\n")
        units(est_time) <- "secs"

        if (!is.null(self$timings$se$end)) {
          se_time <- self$timings$se$end - self$timings$se$start
          cat("Std. Errors took", format(se_time), "\n")
          units(se_time) <- "secs"
        }
        invisible(data.frame(
          "Estimation"  = est_time,
          "Std. Errors" = ifelse(is.null(self$timings$se$end), NA, se_time)
        ))
      } else {
        cat("No timing information.")
      }
    }
  ),
  private = list(
    # control parameters
    control      = list(
      iter     = 0L,
      max_iter = 1e4L,
      tol      = 1e-8,
      se       = TRUE
    ),

    # slots
    X               = NULL, # model matrix
    y               = NULL, # outcome var
    N               = NULL, # sample size
    P               = NULL, # number of preds
    Pp              = NULL, # partner number of preds

    betas           = NULL, # max_iter*P vector of parameters
    pred_incoming   = NULL, # N*max_iter matrix of the incoming predictions
    pred_outgoing   = NULL, # N*max_iter matrix of the outgoing predictions

    # callback
    run_callback    = function() {
      # reset the callback first and then run it
      cb <- self$callback
      self$callback <- function() invisible(NULL)
      do.call(cb, args = list())
    },

    # estimation
    fit_model       = function() {
      if (self$debug) cat(paste(self$name, "| Computing beta.\n"))
      pred_in  <- private$pred_incoming[, private$control$iter]
      pred_out <- private$pred_outgoing[, max(1, private$control$iter - 1)]

      glm_fit  <- glm.fit(private$X, private$y, intercept = FALSE,
                          family = self$family, offset = pred_in)
      self$beta <- as.vector(glm_fit[["coefficients"]])

      # self$beta  <- switch(self$family,
      #   gaussian = fit_gaussian(private$y, private$X, pred_in),
      #   binomial = fit_binomial(private$y, private$X, pred_in)
      # )

      private$betas[, private$control$iter] <- self$beta
    },
    compute_pred    = function() {
      if (self$debug) cat(paste(self$name, "| Computing prediction.\n"))
      private$pred_outgoing[,private$control$iter] <- private$X %*% self$beta
    },
    send_pred       = function(type) {
      if (self$debug) cat(paste(self$name, "| Sending prediction.\n"))
      private$send_message(type = type, data = private$pred_outgoing[, private$control$iter])
    },
    run_estimate    = function() {
      if (is.null(self$timings$estimate$start))
        self$timings$estimate$start <- Sys.time()
      private$control$iter <- private$control$iter + 1L
      private$pred_incoming[,private$control$iter] <- private$msg_incoming$data
      private$fit_model()
      private$compute_pred()
      if (self$verbose) {
        cat(self$name,
            "| iteration:", private$control$iter,
            "| deviance:", format(self$deviance(), nsmall = 8), "\n")
      }

      if (self$converged()) {
        message("PrivReg converged")
        self$timings$estimate$end <- Sys.time()
        private$send_pred(type = "final_iter")
        if (private$control$se) private$compute_se()
        private$run_callback()
      } else if (private$control$iter < private$control$max_iter) {
        private$send_pred(type = "estimate")
      } else {
        message("Maximum iterations reached")
        self$timings$estimate$end <- Sys.time()
        private$send_pred(type = "final_iter")
        if (private$control$se) private$compute_se()
        private$run_callback()
      }
    },
    final_estimate  = function() {
      private$control$iter <- private$control$iter + 1L
      private$pred_incoming[,private$control$iter] <- private$msg_incoming$data
      private$fit_model()
      private$compute_pred()
      self$timings$estimate$end <- Sys.time()
      if (private$control$iter == private$control$max_iter) {
        message("Maximum iterations reached")
      } else {
        message("Partner converged")
        private$control$iter <- private$control$iter + 1L
        private$pred_incoming[,private$control$iter] <- private$msg_incoming$data
        private$fit_model()
        private$compute_pred()
      }
      if (private$control$se) private$compute_se()
      private$run_callback()
    },

    # SE computation
    send_p          = function() {
      if (self$debug) cat(self$name, "| Sending P.\n")
      private$send_message("send_p", data = private$P)
    },
    return_p        = function() {
      if (self$debug) cat(self$name, "| Receiving and returning P.\n")
      private$Pp <- private$msg_incoming$data
      private$send_message("return_p", data = private$P)
    },
    receive_p       = function() {
      if (self$debug) cat(self$name, "| Receiving P.\n")
      private$Pp <- private$msg_incoming$data
      # start the estimation
      private$send_pred(type = "estimate")
    },
    compute_se      = function() {
      if (self$verbose) cat(paste(self$name, "| Computing standard errors.\n"))
      self$timings$se$start <- Sys.time()
      R   <- private$control$iter
      fam <- self$family

      # Partitioned linear prediction matrices
      # we only need min(R, N) iterations because the max rank is N.
      idx       <- seq(1, R, length.out = min(R, private$N))
      Eta_self  <- private$pred_outgoing[, idx]
      Eta_other <- private$pred_incoming[, idx]
      Eta       <- Eta_self + Eta_other

      # Outcome matrix
      Y <- matrix(private$y, private$N, min(R, private$N))

      # Other necessary glm matrices
      Mu    <- apply(Eta, 2, fam$linkinv) # mu matrix - mean of response
      Delta <- apply(Eta, 2, fam$mu.eta)  # dmu/deta matrix

      # Also perform this for the converged iteration (note lowercase letters)
      eta   <- private$pred_outgoing[, R] + private$pred_incoming[, R]
      mu    <- fam$linkinv(eta)
      delta <- fam$mu.eta(eta)
      vari  <- fam$variance(mu)
      w     <- sqrt(delta^2 / vari)

      # residual matrix on the working response level
      Eps_self <- Eta_other + (Y - Mu) / Delta

      # Weighted hat matrix
      Hhat  <- (Eta_other) %*% MASS::ginv(Eps_self)

      # Eigendecomposition with max Pp eigenvalues
      eig        <- RSpectra::eigs(Hhat, k = private$Pp)
      Hhat_range <- private$determine_range(eig$values)

      # Rotated Xpartner matrix
      RXp <- eig$vectors[, 1:Hhat_range]

      # Augmented X matrix
      aX <- cbind(private$X, RXp)

      # unscaled VCOV
      covmat_unscaled <- solve(crossprod(aX*w))[1:private$P, 1:private$P]

      # dispersion correction
      df_r <- private$N - private$P - private$Pp
      if (self$family$family %in% c("poisson", "binomial")) {
        dispersion <- 1
      } else if (df_r > 0) {
        dispersion <- c(crossprod(self$residuals())) / df_r
      } else {
        dispersion <- NaN
      }
      covmat <- dispersion * covmat_unscaled

      self$SE <- Re(sqrt(diag(covmat)))
      if (self$verbose) cat(paste(self$name, "| Done!\n"))
      self$timings$se$end <- Sys.time()
    },
    determine_range = function(ev) {
      # determine how many eigenvectors should go into Z based on eigenvalues
      zap11 <- sum(zapsmall(Mod(ev), 11) > 0)

      return(zap11)
    },

    # networking
    ws              = NULL, # the websocket object
    srv             = NULL, # the httpuv server object
    setup_ws_server = function() {
      # httpuv websocket has different api than websocket :(
      # see https://github.com/rstudio/httpuv/blob/12744e32b0ef8480d8cffb12e9888cbae7f12778/R/httpuv.R#L291
      if (self$debug) cat(paste(self$name, "| setting up ws.\n"))

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
      if (self$debug) cat(paste(self$name, "| setting up ws.\n"))

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
        if (self$debug)
          cat(paste(self$name, "| Transmit data > 30MB, chunking ... \n"))
        for (i in 2:cut_len) {
          bits <- msg_enc[(cuts[i - 1] + 1):cuts[i]]
          type <- ifelse(i == cut_len, "chunk_last", "chunk")
          part_msg_raw <- list(type = type, no = i - 1, bits = bits)
          part_msg_enc <- private$data_encrypt(part_msg_raw, self$crypt_key)
          private$ws$send(part_msg_enc)
          if (self$debug)
            cat(paste(self$name, "|", i - 1, "/", cut_len - 1, "\n"))
        }
      }

    },
    receive_message = function() {
      private$msg_incoming <-
        private$data_decrypt(private$msg_enc_in, self$crypt_key)

      # chunking stuff
      if (private$msg_incoming$type == "chunk") {
        # message is chunked!
        no <- private$msg_incoming$no
        if (self$debug)
          cat(paste(self$name, "| Receiving chunked message,", no, "\n"))
        private$msg_chunks_in[[no]] <- private$msg_incoming$bits
        return()
      }
      if (private$msg_incoming$type == "chunk_last") {
        no <- private$msg_incoming$no
        if (self$debug)
          cat(paste(self$name, "| Receiving chunked message,", no, "(end)\n"))
        private$msg_enc_in <- c(unlist(private$msg_chunks_in),
                                private$msg_incoming$bits)
        private$msg_chunks_in <- NULL
        private$receive_message()
        return()
      }

      if (self$debug)
        cat(paste(self$name, "|", private$msg_incoming$type, "\n"))

      switch(private$msg_incoming$type,
        "estimate"   = private$run_estimate(),
        "final_iter" = private$final_estimate(),
        "send_p"     = private$return_p(),
        "return_p"   = private$receive_p()
      )
    }
  )
)
