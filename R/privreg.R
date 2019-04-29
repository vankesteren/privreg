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
    control       = list(
      iter          = 0L,
      boot_iter     = 0L,
      max_iter      = 1e3L,
      tol           = 1e-8,
      boot_tol      = 1e-5
    ),
    verbose       = NULL,
    name          = NULL,
    crypt_key     = NULL,
    initialize    = function(formula, data, family = "gaussian", name = "alice",
                             verbose = FALSE, crypt_key = "testkey") {
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
      private$betas         <- matrix(0, nrow = self$control$max_iter, ncol = private$P)
      private$pred_incoming <- rep(0, private$N)
      private$pred_outgoing <- rep(0, private$N)
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
      self$control$iter <- self$control$iter + 1L
      private$compute_pred()
      private$send_pred(type = "estimate")
    },
    bootstrap     = function(R = 1000) {
      if (!self$connected()) stop("PrivReg disconnected. Please reconnect first.")
      private$R <- R
      private$setup_bootstrap()
      private$send_message("start_bootstrap", boot_idx_mat = private$boot_idx_mat)
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
      } else if (is.null(private$boot_beta)) {
        tab <- cbind(self$beta)
        rownames(tab) <- colnames(private$X)
        colnames(tab) <- "est"
      } else {
        se <- apply(private$boot_beta[private$boot_converged,], 2, sd)
        qq <- apply(private$boot_beta[private$boot_converged,], 2, quantile,
                    c(0.025, 0.975))
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
        "   family:      ", self$family, "\n",
        "   formula:     ", frml[2], " ", frml[1]," ", frml[3], "\n",
        "   iterations:  ", self$control$iter, "\n",
        "   bootstrap R: ", sum(private$boot_converged), "\n\n",
        "   Parameter estimates\n",
        "   -------------------\n\n"
      )
      print(round(tab, 4))
      invisible(tab)
    }
  ),
  private = list(
    # slots
    X               = NULL, # model matrix
    y               = NULL, # outcome var
    N               = NULL, # sample size
    P               = NULL, # number of preds
    R               = NULL, # bootstrap replicates

    betas           = NULL, # parameters
    pred_incoming   = NULL, # the incoming predictions
    pred_outgoing   = NULL, # the outgoing predictions

    boot_beta       = NULL, # replicated parameters
    boot_idx_mat    = NULL, # bootstrap indices
    boot_pred_in    = NULL, # boot pred mat incoming
    boot_pred_out   = NULL, # boot pred mat outgoing
    boot_converged  = NULL, # converg per replication

    # estimation
    run_estimate    = function() {
      private$pred_incoming <- private$msg_incoming$data
      private$fit_model()
      private$compute_pred()
      self$control$iter <- self$control$iter + 1L
      if (self$verbose) cat(self$name, "| iteration:", self$control$iter, "\n")
      if (self$converged()) {
        message("PrivReg converged")
        private$send_pred(type = "final_iter")
      } else if (self$control$iter < self$control$max_iter) {
        private$send_pred(type = "estimate")
      } else {
        message("Maximum iterations reached")
        private$send_pred(type = "final_iter")
      }
    },
    final_estimate  = function() {
      private$pred_incoming <- private$msg_incoming$data
      private$fit_model()
      self$control$iter <- self$control$iter + 1L
      private$compute_pred()
      if (self$control$iter == self$control$max_iter) {
        message("Maximum iterations reached")
      } else {
        message("Partner converged")
      }
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

    # bootstrapping
    setup_bootstrap = function() {
      partner <- !is.null(private$msg_incoming[["boot_idx_mat"]])
      if (!partner) {
        # create a bootstrap matrix
        private$boot_idx_mat <- private$create_boot_idx()
      } else {
        # use the partner boot matrix and set R
        private$boot_idx_mat <- private$msg_incoming[["boot_idx_mat"]]
        private$R            <- nrow(private$boot_idx_mat)
      }

      # make space for boot betas
      private$boot_beta      <- matrix(self$beta, nrow = private$R,
                                       ncol = private$P, byrow = TRUE)
      private$boot_pred_in   <- matrix(0, nrow = private$R, ncol = private$N)
      private$boot_pred_out  <- matrix(0, nrow = private$R, ncol = private$N)
      private$boot_converged <- rep(FALSE, private$R)

      if (partner) {
        # first iteration of bootstrap
        private$boot_pred_in <- matrix(private$y, private$R, private$N,
                                       byrow = TRUE)
        private$fit_boot_beta()
        private$make_boot_pred()
        self$control$boot_iter <- self$control$boot_iter + 1L
        private$send_boot_pred(type = "bootstrap")
      }
    },
    create_boot_idx = function() {
      t(sapply(1:private$R, function(r) sample(private$N, replace = TRUE)))
    },
    run_bootstrap   = function() {
      private$boot_pred_in <- private$msg_incoming$data
      private$fit_boot_beta()
      private$make_boot_pred()
      self$control$boot_iter <- self$control$boot_iter + 1L
      if (self$verbose) cat(self$name, "| iteration:", self$control$boot_iter, "\n")
      if (self$verbose) cat(self$name, "| converged:", sum(private$boot_converged), "\n")
      if (all(private$boot_converged)) {
        message("All boot samples converged")
        private$send_boot_pred(type = "final_boot")
      } else if (self$control$boot_iter < self$control$max_iter) {
        private$send_boot_pred(type = "bootstrap")
      } else {
        message("Maximum bootstrap iteration reached")
        private$send_boot_pred(type = "final_boot")
      }
    },
    final_bootstrap = function() {
      private$boot_pred_in <- private$msg_incoming$data
      private$fit_boot_beta()
      self$control$boot_iter <- self$control$boot_iter + 1L
      if (self$control$boot_iter == self$control$max_iter) {
        message("Maximum bootstrap iteration reached")
      } else {
        message("Partner converged. Here: ", sum(private$boot_converged),
                "/", private$R)
      }
    },
    fit_boot_beta   = function() {
      if (self$verbose) cat(paste(self$name, "| Computing bootstrap beta.\n"))

      for (r in 1:private$R) {
        if (private$boot_converged[r]) next

        # estimate
        idx <- private$boot_idx_mat[r,]
        boot_beta_r <- switch(self$family,
          gaussian = fit_gaussian(private$y[idx], private$X[idx,],
                                  private$boot_pred_in[r,]),
          binomial = fit_binomial(private$y[idx], private$X[idx,],
                                  private$boot_pred_in[r,],
                                  private$boot_pred_out[r,])
        )

        # convergence check
        diffs <- abs(private$boot_beta[r,] - boot_beta_r)
        if (all(diffs < self$control$boot_tol)) {
          private$boot_converged[r] <- TRUE
        }

        # assign
        private$boot_beta[r,] <- boot_beta_r
      }
    },
    make_boot_pred  = function() {
      if (self$verbose) cat(paste(self$name, "| Computing bootstrap preds.\n"))

      for (r in 1:private$R) {
        idx <- private$boot_idx_mat[r,]
        private$boot_pred_out[r,] <- private$X[idx,] %*% private$boot_beta[r,]
      }
    },
    send_boot_pred  = function(type) {
      if (self$verbose) cat(paste(self$name, "| Sending prediction.\n"))
      private$send_message(type = type, data = private$boot_pred_out)
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

    # communication
    msg_incoming    = NULL, # the incoming message
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
      private$ws$send(msg_enc)
    },
    receive_message = function() {
      if (self$verbose)
        cat(paste(self$name, "|", private$msg_incoming$type, "\n"))

      switch(private$msg_incoming$type,
        "start_bootstrap" = private$setup_bootstrap(),
        "bootstrap"       = private$run_bootstrap(),
        "estimate"        = private$run_estimate(),
        "final_iter"      = private$final_estimate(),
        "final_boot"      = private$final_bootstrap()
      )
    }
  )
)
