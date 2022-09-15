#!chezscheme
(library (tcp)
  (export
   process-tcpsession)
  (import
   (chezscheme)
   (cipher)
   (common)
   (swish imports)
   (tools))

  (define-values (tcp-bufpool-get! tcp-bufpool-putback!) (make-bufpool))

  (define (process-tcpsession ip op bv)
    (define proxy (get-proxy bv))
    (if proxy
        (let ([host (car proxy)]
              [port (cdr proxy)])
          (when host
            (printf "proxy host: ~a:~a~%" host port)
            (unless port (set! port 80))
            (match
             (try
              (let-values ([(dip dop) (connect-tcp host port)])
                (list 'result dip dop)))
              [`(catch ,_)
               (put-bytevector op
                 (string->utf8
                  (string-append "Proxy address [" host ":" port "] ResolveTCP() error")))
               (flush-output-port op)]
              [(result ,dip ,dop)
               ;; start tcp forward
               (spawn (lambda () (tcp-forward dip op)))
               (tcp-forward ip dop)
               (close-output-port dop)
               (close-input-port dip)])))
        (begin
          (put-bytevector-some op (string->utf8 "No proxy host"))
          (flush-output-port op))))

  (define (tcp-forward ip op)
    (let ([bv (tcp-bufpool-get! (tcp-buffer-size))])
      (let lp ([n (get-bytevector-some! ip bv 0 (tcp-buffer-size))]
               [subi 0])
        (unless (eof-object? n)
          (let ([rem (decrypt-data! bv subi n)])
            (put-bytevector-some op bv 0 n)
            (flush-output-port op)
            (lp (get-bytevector-some! ip bv 0 (tcp-buffer-size)) rem))))
      (tcp-bufpool-putback! bv)))

  (define *host-re* (re "\\s*:\\s*"))
  (define (get-proxy bv)
    (let ([start (bytevector-u8-index bv (string->utf8 (proxy-key)))])
      (and start
           (let ([end (bytevector-u8-index bv start (string->utf8 "\r"))])
             (and end
                  (let* ([proxy-line (subbytevector bv start end)]
                         [rs (pregexp-split *host-re* (utf8->string proxy-line))])
                    (and (>= (length rs) 2)
                         (let* ([host-port (decrypt-host (string->utf8 (cadr rs)))]
                                [host-and-port (pregexp-split ":" (utf8->string host-port))])
                           (and (>= (length host-and-port) 2)
                                (let* ([port (cadr host-and-port)])
                                  (cons (car host-and-port) (substring port 0 (- (string-length port) 1)))))))))))))

  )
