#!chezscheme
(library (common)
  (export
   decrypt-data!
   decrypt-host
   host
   host-regex
   port
   secret
   tcp-buffer-size
   tcp-queue-size
   buffer-pool-fixed?
   http-flag
   http-header?
   response-header
   set-config!
   logger-on?
   tcp-buf-queue)
  (import
   (chezscheme)
   (cipher)
   (swish imports)
   (tools))

  (define host (make-parameter #f))
  (define port (make-parameter #f))
  (define host-regex (make-parameter #f))
  (define secret (make-parameter #f))
  (define http-flag (make-parameter #f))
  (define tcp-buffer-size (make-parameter #f))
  (define tcp-queue-size (make-parameter #f))
  (define buffer-pool-fixed? (make-parameter #f))
  (define logger-on? (make-parameter #f))

  (define tcp-buf-queue (make-parameter #f))


  ;; setting up global configurations
  (define (set-config! file)
    (let* ([ss (with-input-from-file file read)]
           [proxy-key (cdr (assoc 'proxy-key ss))])
      (host (cdr (assoc 'host ss)))
      (port (cdr (assoc 'port ss)))
      (host-regex (re (string-append proxy-key ":\\s*(.+)\\r")))
      (secret (cdr (assoc 'secret ss)))
      (http-flag (cdr (assoc 'http-flag ss)))
      (tcp-buffer-size (cdr (assoc 'tcp-buffer-size ss)))
      (tcp-queue-size (cdr (assoc 'tcp-queue-size ss)))
      (buffer-pool-fixed? (cdr (assoc 'buffer-pool-fixed? ss)))
      (heap-reserve-ratio (cdr (assoc 'heap-reserve-ratio ss)))
      (logger-on? (cdr (assoc 'logger-on? ss))))
    (tcp-buf-queue
     (let ([q (make-queue)])
       (q 'set!
         (make-bufpool (tcp-buffer-size) (tcp-queue-size)))
       q)))

  (define decrypt-data!
    (case-lambda
     [(bv) (decrypt-data! bv 0 (bytevector-length bv))]
     [(bv subi len) (xor-cipher! bv (secret) subi len)]))

  (define (decrypt-host bvhost)
    (let ([host (base64-decode-bytevector bvhost)])
      (xor-cipher! host (secret))
      host))

  (define *headers*
    (list "CONNECT" "GET" "POST" "HEAD" "PUT" "COPY"
      "DELETE" "MOVE" "OPTIONS" "LINK" "UNLINK"
      "TRACE" "WRAPPER"))

  (define (http-header? header)
    (ormap (lambda (h)
             (starts-with-ci? header h))
      *headers*))

  (define (response-header header)
    (cond
     [(string-contains-ci? header "websocket")
      (string->utf8 "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: CuteBi Network Tunnel, (%>w<%)\r\n\r\n")]
     [(starts-with-ci? header "CON")
      (string->utf8 "HTTP/1.1 200 Connection established\r\nServer: CuteBi Network Tunnel, (%>w<%)\r\nConnection: keep-alive\r\n\r\n")]
     [else
      (string->utf8 "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nServer: CuteBi Network Tunnel, (%>w<%)\r\nConnection: keep-alive\r\n\r\n")]))

  )


#!eof mats
(import
 (common)
 (tools))
(isolate-mat common-test ()
  (let ([content1 "GET / HTTP/1.1\r\n"]
        [content2 "WS / HTTP/1.1\r\n"])
    (printf "content1: ~a~n" (http-header? content1))
    (printf "content2: ~a~n" (http-header? content2))))
