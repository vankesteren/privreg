#' Private regression with vertically partitioned data
#'
#' This class sets up the connection and performs estimation for private regression.
#'
#' @export
privreg <- R6::R6Class(
  classname = "privreg",
  public = list(
    beta = NULL,
    iter = 0L,
    verbose = NULL,
    max_iter = 1e3L,
    name = NULL,
    initialize = function(X, y, name = "alice", verbose = FALSE) {
      private$X <- X
      private$y <- y
      self$beta <- numeric(ncol(X))
      self$name <- name
      self$verbose <- verbose
    },
    listen = function(port = 8080) {
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
    connect = function(url, port = 8080) {
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
    start = function() {
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
    }
  ),
  private = list(
    X = NULL,
    y = NULL,
    y_res_incoming = NULL,
    y_res_outgoing = NULL,
    ws = NULL,
    srv = NULL,

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

        private$y_res_incoming <-  private$encrypted_raw_to_vec(message)
        private$compute_beta()
        self$iter = self$iter + 1L
        if (self$verbose) cat(self$name, "| iteration:", self$iter, "\n")
        if (self$iter < self$max_iter) {
          private$compute_y_res()
          private$send_y_res()
        } else {
          self$disconnect()
        }
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
        private$y_res_incoming <- private$encrypted_raw_to_vec(event$data)
        private$compute_beta()
        self$iter = self$iter + 1L
        if (self$verbose) cat(self$name, "| iteration:", self$iter, "\n")
        if (self$iter < self$max_iter) {
          private$compute_y_res()
          private$send_y_res()
        } else {
          self$disconnect()
        }
      })

      private$ws$onClose(function(event) {
        cat(self$name, "| Connection closed.\n")
      })
    },

    compute_beta = function() {
      if (self$verbose) cat(paste(self$name, "| computing beta.\n"))
      self$beta <- coef(lm(private$y_res_incoming ~ private$X + 0))
    },

    compute_y_res = function() {
      if (self$verbose) cat(paste(self$name, "| Computing y_res.\n"))
      private$y_res_outgoing <- private$y - private$X %*% self$beta
    },

    send_y_res = function() {
      if (self$verbose) cat(paste(self$name, "| sending y_res.\n"))
      msg <- private$vec_to_encrypted_raw(private$y_res_outgoing)
      private$ws$send(msg)
    },

    vec_to_encrypted_raw = function(vec, key = "test") {
      # converts numeric vector to secure binary message
      serialized <- serialize(vec, NULL)
      return(private$encrypt(serialized, key))
    },
    encrypted_raw_to_vec = function(raw, key = "test") {
      decrypted <- private$decrypt(raw, key)
      unserialize(decrypted)
    },
    encrypt = function(text, key) {
      # TODO: replace this with actual encryption at some point.
      text
    },
    decrypt = function(text, key) {
      # TODO: replace this with actual decryption at some point
      text
    }
  )
)
