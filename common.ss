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
   http-flag
   http-header?
   response-header
   set-config!
   logger-on?)
  (import
   (chezscheme)
   (cipher)
   (swish imports)
   (tools))

  (define host (make-parameter #f))
  (define port (make-parameter #f))
  (define host-regex (make-parameter #t))
  (define secret (make-parameter #f))
  (define http-flag (make-parameter #f))
  (define tcp-buffer-size (make-parameter #f))
  (define logger-on? (make-parameter #t))

  ;;; setting up global configurations
  (define (set-config! file)
    (let* ([ss (with-input-from-file file read)]
           [proxy-key (cdr (assoc 'proxy-key ss))])
      (host (cdr (assoc 'host ss)))
      (port (cdr (assoc 'port ss)))
      (host-regex (re (string-append proxy-key ":\\s*(.+)\\r")))
      (secret (cdr (assoc 'secret ss)))
      (http-flag (cdr (assoc 'http-flag ss)))
      (tcp-buffer-size (cdr (assoc 'tcp-buffer-size ss)))
      (heap-reserve-ratio (cdr (assoc 'heap-reserve-ratio ss)))
      (logger-on? (cdr (assoc 'logger-on? ss)))))

  (define decrypt-data!
    (case-lambda
     [(bv) (decrypt-data! bv 0 (bytevector-length bv))]
     [(bv subi len) (xor-cipher! bv (secret) subi len)]))

  (define (decrypt-host bvhost)
    (let ([host (base64-decode-bytevector bvhost)])
      (xor-cipher! host (secret))
      host))

  ;; '((header . header-length) ...)
  (define *headers*
    (map (lambda (v) (cons (string->utf8 v) (string-length v)))
      '("CONNECT" "GET" "POST" "HEAD" "PUT" "COPY"
        "DELETE" "MOVE" "OPTIONS" "LINK" "UNLINK"
         "TRACE" "WRAPPER")))
  (define *web-socket-header*
    (let ([name "WebSocket"])
      (cons (string->utf8 name) (string-length name))))
  (define *connect-header*
    (let ([name "CON"])
      (cons (string->utf8 name) (string-length name))))

  (define (http-header? bv)
    (ormap (lambda (h)
             (fstarts-with? bytevector-u8-ref bv (car h) (cdr h)))
      *headers*))

  (define (response-header bv)
    (cond
     [(contains bytevector-u8-ref bv (car *web-socket-header*)
        (bytevector-length bv) (cdr *web-socket-header*))
      (string->utf8 "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: CuteBi Network Tunnel, (%>w<%)\r\n\r\n")]
     [(contains bytevector-u8-ref bv (car *connect-header*)
        (bytevector-length bv) (cdr *connect-header*))
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
    (printf "content1: ~a~n" (http-header? (string->utf8 content1)))
    (printf "content2: ~a~n" (http-header? (string->utf8 content2)))))
