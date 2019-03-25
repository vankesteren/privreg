#' Private regression with vertically partitioned data
#'
#' This class sets up the connection and performs estimation for private regression.
#'
#' @export
PrivReg <- R6::R6Class(
  classname = "PrivReg",
  public = list(
    beta       = NULL,
    family     = "gaussian",
    iter       = 0L,
    verbose    = NULL,
    max_iter   = 1e3L,
    name       = NULL,
    crypt_key  = NULL,
    initialize = function(formula, data, family = "gaussian", name = "alice",
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
    },
    listen     = function(port = 8080) {
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
    connect    = function(url, port = 8080) {
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
    start      = function() {
      if (!self$connected()) stop("Connect to another institution first.")
      if (self$verbose) cat(paste(self$name, "| Performing initial run\n"))
      private$fit_model()
      self$iter <- self$iter + 1L
      private$compute_pred()
      private$send_pred()
    },
    disconnect = function() {
      if (self$verbose) cat(paste(self$name, "| Disconnecting.\n"))
      if (inherits(private$ws, "WebSocket")) private$ws$close()
      if (!is.null(private$srv)) {
        httpuv::stopServer(private$srv)
        private$srv <- NULL
      }
    },
    connected  = function() {
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
    plot_paths = function() {
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
        ggplot2::ggplot(aes(x = iter, y = value, colour = param)) +
        ggplot2::geom_line(size = 1) +
        firatheme::theme_fira() +
        firatheme::scale_colour_fira()
    }
  ),
  private = list(
    X               = NULL,
    y               = NULL,
    betas           = NULL,
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
        private$run_iteration(message)
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
        private$run_iteration(event$data)
      })

      private$ws$onClose(function(event) {
        cat(self$name, "| Connection closed.\n")
      })
    },
    run_iteration   = function(message) {
      private$pred_incoming <- private$data_decrypt(message, self$crypt_key)
      private$fit_model()
      self$iter <- self$iter + 1L
      if (self$verbose) cat(self$name, "| iteration:", self$iter, "\n")
      if (self$iter < self$max_iter) {
        private$compute_pred()
        private$send_pred()
      } else {
        self$disconnect()
      }
    },
    fit_model       = function() {
      if (self$verbose) cat(paste(self$name, "| Computing beta.\n"))
      self$beta  <- switch(self$family,
        gaussian = fit_gaussian(private$y, private$X, private$pred_incoming),
        binomial = fit_binomial(private$y, private$X,
                                private$pred_incoming, private$pred_outgoing)
      )
      private$betas[self$iter + 1, ] <- self$beta
    },
    compute_pred    = function() {
      if (self$verbose) cat(paste(self$name, "| Computing prediction.\n"))
      private$pred_outgoing <- private$X %*% self$beta
    },
    send_pred       = function() {
      if (self$verbose) cat(paste(self$name, "| Sending prediction.\n"))
      msg <- private$data_encrypt(private$pred_outgoing, self$crypt_key)
      private$ws$send(msg)
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
    }
  )
)
