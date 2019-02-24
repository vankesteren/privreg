#' Private regression with vertically partitioned data
#'
#' This class sets up the connection and performs estimation for private regression.
#'
#' @export
PrivReg <- R6::R6Class(
  classname = "PrivReg",
  public = list(
    beta       = NULL,
    iter       = 0L,
    verbose    = NULL,
    max_iter   = 1e3L,
    name       = NULL,
    crypt_key  = NULL,
    initialize = function(X, y, name = "alice", verbose = FALSE,
                          crypt_key = "testkey") {
      private$X      <- X
      private$y      <- y
      self$beta      <- numeric(ncol(X))
      self$name      <- name
      self$verbose   <- verbose
      self$crypt_key <- crypt_key
    },
    listen     = function(port = 443) {
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
    connect    = function(url, port = 443) {
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
      if (self$verbose) cat(paste(self$name, "| performing initial run\n"))
      private$y_res_incoming <- private$y
      private$compute_beta()
      self$iter = self$iter + 1L
      private$compute_y_res()
      private$send_y_res()
    },
    disconnect = function() {
      if (self$verbose) cat(paste(self$name, "| disconnecting.\n"))
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
    }
  ),
  private = list(
    X               = NULL,
    y               = NULL,
    y_res_incoming  = NULL,
    y_res_outgoing  = NULL,
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
      private$y_res_incoming <- private$data_decrypt(message, self$crypt_key)
      private$compute_beta()
      self$iter = self$iter + 1L
      if (self$verbose) cat(self$name, "| iteration:", self$iter, "\n")
      if (self$iter < self$max_iter) {
        private$compute_y_res()
        private$send_y_res()
      } else {
        self$disconnect()
      }
    },
    compute_beta    = function() {
      if (self$verbose) cat(paste(self$name, "| computing beta.\n"))
      self$beta <- coef(lm(private$y_res_incoming ~ private$X + 0))
    },
    compute_y_res   = function() {
      if (self$verbose) cat(paste(self$name, "| Computing y_res.\n"))
      private$y_res_outgoing <- private$y - private$X %*% self$beta
    },
    send_y_res      = function() {
      if (self$verbose) cat(paste(self$name, "| sending y_res.\n"))
      msg <- private$data_encrypt(private$y_res_outgoing, self$crypt_key)
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
