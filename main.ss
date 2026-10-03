(import
 (cipher)
 (common)
 (tcp)
 (tools)
 (udp))


(define (handle-connection ip op bv)
  (let ([header (guard (x [else #f]) (utf8->string bv))])
    (cond
     [(and header (http-header? header))
      (printf "handle http request...~%")
      (put-bytevector op (response-header header))
      (flush-output-port op)
      (unless (string-contains? header (http-flag))
        (process-tcpsession ip op header))]
     [else
      (printf "handle udp request...~%")
      (process-udpsession ip op bv)])))


(define (server:start ip op)
  (define (reader me)
    (guard (x [else
               (printf "Error processing connection: ~a~%" x)
               (close-input-port ip)
               (close-output-port op)])
      (let ([bv (get-bytevector-some ip)])
        (unless (eof-object? bv)
          (tcp-nodelay op #t)
          (handle-connection ip op bv))))
    (send me `#(done ,ip ,op)))
  (define (init)
    (let ([me self])
      (spawn (lambda () (reader me))))
    `#(ok #f))
  (define (terminate reason state)
    (printf "Connection worker terminated, reason: ~a~%" reason)
    'ok)
  (define (handle-call msg from state) 
    `#(reply #(error bad-call) ,state))
  (define (handle-cast msg state) 
    `#(no-reply ,state))
  (define (handle-info msg state)
    (match msg
      [#(done ,_ ,_)
       ;; Keep process alive or exit cleanly without prematurely closing active sockets
       (printf "Initial handling complete~%")
       `#(no-reply ,state)]
      [_ `#(stop ,msg ,state)]))
  (gen-server:start #f))


(define (start-server:start)
  (define-state-tuple <mserver> listener)
  (define (init)
    (let ([listener (listen-tcp (host) (port) self)])
      (printf "Waiting for connection on port: ~a~%" (listener-port-number listener))
      `#(ok ,(<mserver> make [listener listener]))))
  (define (terminate reason state)
    (printf "Disconnected, reason: ~a~%" reason)
    (close-tcp-listener ($state listener))
    'ok)
  (define (handle-call msg from state) 
    `#(reply #(error bad-call) ,state))
  (define (handle-cast msg state) 
    `#(no-reply ,state))
  (define (handle-info msg state)
    (match msg
      [#(accept-tcp ,_ ,ip ,op)
       (printf "Handling a new connection~%")
       (server:start ip op)
       `#(no-reply ,state)]
      [#(accept-tcp-failed ,_ ,_ ,_)
       (printf "Handling new connection failed~%")
       `#(stop ,msg ,state)]
      [_ `#(no-reply ,state)]))
  (gen-server:start 'mserver))


(define (run-app config-file)
  ;; setup optimize-level
  (optimize-level 3)
  ;; setup the configuration
  (set-config! config-file)
  ;; app supervisor specials
  (app-sup-spec
   (append
    (if (logger-on?) (make-swish-sup-spec (list swish-event-logger)) '())
    `(#(mserver ,start-server:start permanent 1000 worker))))
  ;; start app
  (app:start)
  (receive))


(define *APP_NAME* "cns")
(define *APP_VERSION_NO* "0.02")

(define app-cli
  (cli-specs
   default-help
   [config-file -c --config-file (string "<file>") "specify the configuration file.
if not specify, default use `config.ss`"]
   [version -v --version bool "display version"]))

(let ([opt (parse-command-line-arguments app-cli)])
  (cond
   [(opt 'help)
    (display-help *APP_NAME* app-cli)]
   [(opt 'version)
    (printf "~a v~a~%" *APP_NAME* *APP_VERSION_NO*)]
   [(opt 'config-file)
    (run-app (opt 'config-file))]
   [else
    (printf "Using default configuration file `config.ss`...~%")
    (run-app "config.ss")]))

