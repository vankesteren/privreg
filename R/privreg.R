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
#' alice$bootstrap()
#'
#' alice$summary()
#' alice$plot_paths()
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
#' \code{$bootstrap()} computes bootstrap distribution of parameters using blockwise bootstrap
#' \code{$summary()} displays a summary of the object
#' \code{$plot_paths()} plots the paths of the parameters over the estimation iterations
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
#' # bootstrap
#' alice$bootstrap()
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
    iter          = 0L,
    max_iter      = 1e3L,
    boot_iter     = NULL,
    boot_max_iter = NULL,
    verbose       = NULL,
    name          = NULL,
    crypt_key     = NULL,
    initialize    = function(formula, data, family = "gaussian", name = "alice",
                             verbose = FALSE, crypt_key = "testkey") {
      private$X      <- model.matrix(formula, data)
      private$y      <- unname(model.response(model.frame(formula, data)))
      private$betas  <- matrix(0, nrow = self$max_iter, ncol = ncol(private$X))
      private$pred_incoming <- rep(0, length(private$y))
      private$pred_outgoing <- rep(0, length(private$y))
      self$name      <- name
      self$verbose   <- verbose
      self$crypt_key <- crypt_key
      self$family    <- family
      self$formula   <- formula
    },
    listen        = function(port = 8080) {
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
    connect       = function(url, port = 8080) {
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
    estimate      = function() {
      if (!self$connected()) stop("Connect to another institution first.")
      if (self$verbose) cat(paste(self$name, "| Performing initial run\n"))
      private$fit_model()
      self$iter <- self$iter + 1L
      private$compute_pred()
      private$send_pred(type = "estimate")
    },
    bootstrap     = function(n_sample = 1000) {
      if (!self$connected())
        stop("PrivReg disconnected. Please reconnect first.")

      private$setup_bootstrap(n_sample)
      private$send_message("bootstrap", n_samples = n_sample)
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
    plot_paths    = function() {
      if (!requireNamespace("firatheme", quietly = TRUE))
        stop("Install firatheme: devtools::install_github(\"vankesteren/firatheme\")")
      if (!requireNamespace("ggplot2", quietly = TRUE))
        stop("Install ggplot2")
      if (!requireNamespace("dplyr", quietly = TRUE))
        stop("Install dplyr")
      if (!requireNamespace("tidyr", quietly = TRUE))
        stop("Install tidyr")

      `%>%` <- dplyr::`%>%`
      tibble::as_tibble(private$betas) %>%
        dplyr::mutate(iter = 1:n()) %>%
        tidyr::gather("param", "value", -iter) %>%
        ggplot2::ggplot(ggplot2::aes(x = iter, y = value, colour = param)) +
        ggplot2::geom_line(size = 1) +
        firatheme::theme_fira() +
        firatheme::scale_colour_fira()
    },
    summary       = function() {
      frml <- as.character(self$formula)
      if (is.null(self$beta)) {
        tab <- NULL
      } else if (is.null(self$boot_iter)) {
        tab <- cbind(self$beta)
        rownames(tab) <- colnames(private$X)
        colnames(tab) <- "est"
      } else {
        se <- apply(private$boot_betas, 2, sd)
        qq <- apply(private$boot_betas, 2, function(x) {
          quantile(x, c(0.025, 0.975))
        })
        tt <- self$beta / se
        pp <- pt(-abs(tt), nrow(private$X) - ncol(private$X)) * 2
        tab <- cbind(self$beta, se, tt, pp, t(qq))
        rownames(tab) <- colnames(private$X)
        colnames(tab) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)",
                           "2.5%", "97.5%")
      }
      cat(sep = "", "\n",
        "   Privacy-preserving GLM\n",
        "   ----------------------\n\n",
        "   family:    ", self$family, "\n",
        "   formula:   ", frml[2], " ", frml[1]," ", frml[3], "\n",
        "   iter:      ", self$iter, "\n",
        "   bootstrap: ", self$boot_iter, "\n\n",
        "   Parameter estimates\n",
        "   -------------------\n\n"
      )
      print(round(tab, 4))
      invisible(tab)
    }
  ),
  private = list(
    X               = NULL,
    y               = NULL,
    betas           = NULL,
    boot_betas      = NULL,
    msg_incoming    = NULL,
    pred_incoming   = NULL,
    pred_outgoing   = NULL,
    ws              = NULL,
    srv             = NULL,
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

        private$msg_incoming <- private$data_decrypt(message, self$crypt_key)
        private$receive_message()
      })

      private$ws$onClose(function() {
        cat(self$name, "| Connection closed.\n")
      })
    },
    setup_ws_client = function() {
      # websocket has different api than httuv websocket :(
      if (self$verbose) cat(paste(self$name, "| setting up ws.\n"))

      private$ws$onOpen(function(event) {
        cat(self$name, "| Connection opened.\n")
      })

      private$ws$onMessage(function(event) {
        private$msg_incoming <- private$data_decrypt(event$data, self$crypt_key)
        private$receive_message()
      })

      private$ws$onClose(function(event) {
        cat(self$name, "| Connection closed.\n")
      })
    },
    setup_bootstrap = function(n_sample) {
      # create a bootstrap matrix
      private$boot_betas <- matrix(0, nrow = n_sample, ncol = length(self$beta))
      self$boot_max_iter <- n_sample
      self$boot_iter     <- 0L
    },
    run_estimate    = function() {
      private$pred_incoming <- private$msg_incoming$data
      private$fit_model()
      private$compute_pred()
      self$iter <- self$iter + 1L
      if (self$verbose) cat(self$name, "| iteration:", self$iter, "\n")
      if (self$iter < self$max_iter) {
        private$send_pred(type = "estimate")
      } else {
        message("Maximum iterations reached")
        private$send_pred(type = "final_iter")
      }
    },
    final_estimate  = function() {
      private$pred_incoming <- private$msg_incoming$data
      private$fit_model()
      self$iter <- self$iter + 1L
      private$compute_pred()
      message("Maximum iterations reached")
    },
    run_bootstrap   = function() {
      if (!is.null(private$msg_incoming$n_samples)) {
        private$setup_bootstrap(private$msg_incoming$n_samples)
      } else {
        private$pred_incoming <- private$msg_incoming$data
      }
      private$fit_boot_beta()
      private$compute_pred()
      self$boot_iter <- self$boot_iter + 1L
      if (self$verbose) cat(self$name, "| iteration:", self$boot_iter, "\n")
      if (self$boot_iter < self$boot_max_iter) {
        private$send_pred(type = "bootstrap")
      } else {
        message("Maximum bootstrap iteration reached")
        self$beta <- private$betas[self$iter, ]
        private$send_pred(type = "final_boot")
      }
    },
    final_bootstrap = function() {
      private$pred_incoming <- private$msg_incoming$data
      private$fit_boot_beta()
      self$boot_iter <- self$boot_iter + 1L
      self$beta <- private$betas[self$iter, ]
      message("Maximum bootstrap iteration reached")
    },
    fit_model       = function() {
      if (self$verbose) cat(paste(self$name, "| Computing beta.\n"))
      self$beta  <- switch(self$family,
        gaussian = fit_gaussian(private$y, private$X,
                                private$pred_incoming),
        binomial = fit_binomial(private$y, private$X,
                                private$pred_incoming,
                                private$pred_outgoing)
      )
      private$betas[self$iter + 1, ] <- self$beta
    },
    fit_boot_beta   = function() {
      if (self$verbose) cat(paste(self$name, "| Computing bootstrap beta.\n"))
      idx <- sample(nrow(private$X), replace = TRUE)
      self$beta  <- switch(self$family,
        gaussian = fit_gaussian(private$y[idx], private$X[idx,],
                                private$pred_incoming[idx]),
        binomial = fit_binomial(private$y[idx], private$X[idx,],
                                private$pred_incoming[idx],
                                private$pred_outgoing[idx])
      )
      private$boot_betas[self$boot_iter + 1, ] <- self$beta
    },
    compute_pred    = function() {
      if (self$verbose) cat(paste(self$name, "| Computing prediction.\n"))
      private$pred_outgoing <- private$X %*% self$beta
    },
    send_pred       = function(type) {
      if (self$verbose) cat(paste(self$name, "| Sending prediction.\n"))
      msg_raw <- list(type = type, data = private$pred_outgoing)
      msg_enc <- private$data_encrypt(msg_raw, self$crypt_key)
      private$ws$send(msg_enc)
    },
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
    send_message    = function(message, ...) {
      msg_raw <- list(type = message, ...)
      msg_enc <- private$data_encrypt(msg_raw, self$crypt_key)
      private$ws$send(msg_enc)
    },
    receive_message = function() {
      if (self$verbose)
        cat(paste(self$name, "|", private$msg_incoming$type, "\n"))

      switch(private$msg_incoming$type,
        "bootstrap"  = private$run_bootstrap(),
        "estimate"   = private$run_estimate(),
        "final_iter" = private$final_estimate(),
        "final_boot" = private$final_bootstrap()
      )
    }
  )
)
