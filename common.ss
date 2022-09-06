#!chezscheme
(library (common)
  (export
   decrypt-data!
   decrypt-host
   get-host
   get-proxy-key
   get-port
   get-secret
   get-tcp-buffer-size
   http-header?
   response-header
   set-config!
   get-http-flag)
  (import
   (chezscheme)
   (cipher)
   (swish imports)
   (tools))

  (define *host #f)
  (define *port #f)
  (define *proxy-key #f)
  (define *secret #f)
  (define *http-flag #f)
  (define *tcp-buffer-size #f)

  (define (get-host) *host)
  (define (get-port) *port)
  (define (get-proxy-key) *proxy-key)
  (define (get-secret) *secret)
  (define (get-http-flag) *http-flag)
  (define (get-tcp-buffer-size) *tcp-buffer-size)

  ;;; setting up global configurations
  (define (set-config! file)
    (let ([ss (with-input-from-file file (lambda () (read)))])
      (set! *host (cdr (assoc 'host ss)))
      (set! *port (cdr (assoc 'port ss)))
      (set! *proxy-key (cdr (assoc 'proxy-key ss)))
      (set! *secret (cdr (assoc 'secret ss)))
      (set! *http-flag (cdr (assoc 'http-flag ss)))
      (set! *http-flag (cdr (assoc 'tcp-buffer-size ss)))))

  (define decrypt-data!
    (case-lambda
     [(bv) (decrypt-data! bv 0)]
     [(bv subi) (xor-cipher! bv (get-secret) subi)]))

  (define (decrypt-host bvhost)
    (let ([host (base64-decode-bytevector bvhost)])
      (xor-cipher! host (get-secret))
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
