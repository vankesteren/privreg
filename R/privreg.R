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
          onHeaders = function(req) { cat(capture.output(str(as.list(req))), sep = "\n") },
          onWSOpen = function(ws) {
            cat("Connection opened.\n")
            private$ws <- ws
            private$setup_ws_server()
          }
        )
      )
    },
    connect = function(url, port = 8080) {
      if (self$verbose) cat(paste(self$name, "| opening websocket connection\n"))
      private$ws <- websocket::WebSocket$new(
        url = url,
        headers = list(Cookie = "private_regression"),
        accessLogChannels = "none",
        autoConnect = FALSE
      )
      private$setup_ws_connect()
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
      if (self$verbose) cat(paste(self$name, "| setting up ws.\n"))
      private$ws$onMessage(function(binary, message) {
        private$y_res_incoming <- c(eval(parse(text = rawToChar(message))))
        private$compute_beta()
        self$iter = self$iter + 1L
        if (self$verbose) cat(self$iter, "\n")
        if (self$iter < self$max_iter) {
          private$compute_y_res()
          private$send_y_res()
        } else {
          self$disconnect()
        }
      })
      private$ws$onClose(function() {
        cat("Connection closed.\n")
      })
    },

    setup_ws_connect = function() {
      if (self$verbose) cat(paste(self$name, "| setting up ws.\n"))
      private$ws$onOpen(function(event) {
        cat("connection opened\n")
      })

      private$ws$onMessage(function(event) {
        private$y_res_incoming <- c(eval(parse(text = rawToChar(event$data))))
        private$compute_beta()
        self$iter = self$iter + 1L
        if (self$verbose) cat(self$iter)
        if (self$iter < self$max_iter) {
          private$compute_y_res()
          private$send_y_res()
        } else {
          self$disconnect()
        }
      })

      private$ws$onClose(function(event) {
        cat("Client disconnected with code ", event$code,
            " and reason ", event$reason, "\n", sep = "")
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
      msg <- charToRaw(paste(capture.output(dput(private$y_res_outgoing)),
                             collapse = " "))
      private$ws$send(msg)
    }
  )
)
