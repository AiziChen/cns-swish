(import
 (cipher)
 (common)
 (tcp)
 (tools)
 (udp))

(define (handle-connection ip op bv)
  (cond
   [(http-header? bv)
    (printf "handle http request...~%")
    (put-bytevector op (response-header bv))
    (flush-output-port op)
    (let ([http-flag (string->utf8 (get-http-flag))])
      (unless (contains bytevector-u8-ref bv http-flag (bytevector-length http-flag) (string-length (get-http-flag)))
        (match (try (process-tcpsession ip op bv))
          [`(catch ,reason)
           (printf "Process tcp failed, reason: ~a~%" reason)]
          [,_ 'ok])))]
   [else
    (printf "handle udp request...~%")
    (process-udpsession ip op bv)]))


(define (server:start ip op)
  (define (reader who)
    (let ([bv (get-bytevector-some ip)])
      (unless (eof-object? bv)
        (handle-connection ip op bv)
        (send who `#(done ,ip ,op)))))
  (define (init)
    (let ([me self])
      (spawn (lambda () (reader me))))
    `#(ok #f))
  (define (terminate reason state)
    (printf "Connection terminated, reason: ~a~%" reason)
    (close-output-port op)
    'ok)
  (define (handle-call msg from state) (match msg))
  (define (handle-cast msg state) (match msg))
  (define (handle-info msg state)
    (match msg
      [#(done ,ip ,op)
        (close-output-port op)
        (printf "Connection has been closed~%")
       `#(no-reply ,state)]
      [_ `#(stop ,msg ,state)]))
  (gen-server:start #f))


(define (start-server:start)
  (define-state-tuple <mserver> listener)
  (define (init)
    (let ([listener (listen-tcp (get-host) (get-port) self)])
      (printf "Waiting for connection on port: ~a~%" (listener-port-number listener))
      `#(ok ,(<mserver> make [listener listener]))))
  (define (terminate reason state)
    (printf "Disconnected, reason: ~a~%" reason)
    (close-tcp-listener ($state listener))
    'ok)
  (define (handle-call msg from state) (match msg))
  (define (handle-cast msg state) (match msg))
  (define (handle-info msg state)
    (match msg
      [#(accept-tcp ,_ ,ip ,op)
       (printf "Handling a new connection~%")
       (server:start ip op)
       `#(no-reply ,state)]
      [#(accept-tcp-failed ,_ ,_ ,_)
       (printf "Handling new connection falied~%")
       `#(stop ,msg ,state)]))
  (gen-server:start 'mserver))


(define (run-app config-file)
  ;; setup the configuration
  (set-config! config-file)
  ;; app supervisor specials
  (app-sup-spec
   (append
    (make-swish-sup-spec (list swish-event-logger))
    `(#(mserver ,start-server:start permanent 1000 worker))))
  ;; start app
  (app:start)
  (receive))


(define *APP_NAME* "cns-scheme")

(define app-cli
  (cli-specs
   default-help
   [config-file -c --config-file (string "<file>") "specify the configuration file.
if not specify, default use `config.ss`"]
   [version -v --version bool "display version"]))

(let ([opt (parse-command-line-arguments app-cli)])
  (when (opt 'help)
    (display-help *APP_NAME* app-cli)
    (exit 0))
  (when (opt 'version)
    (printf "~a v0.01~%" *APP_NAME*)
    (exit 0))
  (cond
   [(opt 'config-file)
    (run-app (opt 'config-file))]
   [else
    (printf "Using default configuration file `config.ss`...~%")
    (run-app "config.ss")]))

