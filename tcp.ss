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


  (define (process-tcpsession ip op header)
    (define proxy (get-proxy header))
    (if proxy
        (let ([host (vector-ref proxy 0)]
              [port (vector-ref proxy 1)])
          (when host
            (printf "proxy host: ~a:~a~%" host port)
            (unless port (set! port 80))
            (match
             (try
              (let-values ([(dip dop) (connect-tcp host port)])
                `#(result ,dip ,dop)))
             [`(catch ,_)
              (put-bytevector op
                (string->utf8
                 (string-append "Proxy address [" host ":" port "] ResolveTCP() error")))
              (flush-output-port op)]
             [#(result ,dip ,dop)
              ;; start tcp forward
              (spawn&link (lambda () (tcp-forward dip op)))
              (tcp-forward ip dop)])))
        (begin
          (put-bytevector-some op (string->utf8 "No proxy host"))
          (flush-output-port op))))

  (define (tcp-forward ip op)
    (let* ([pool-empty? ((tcp-buf-queue) 'empty?)]
           [bv (if pool-empty?
                   (make-bytevector (tcp-buffer-size))
                   ((tcp-buf-queue) 'drop!))])
      (try
       (let lp ([n (get-bytevector-some! ip bv 0 (tcp-buffer-size))]
                [subi 0])
         (unless (eof-object? n)
           (let ([rem (decrypt-data! bv subi n)])
             (put-bytevector-some op bv 0 n)
             (flush-output-port op)
             (lp (get-bytevector-some! ip bv 0 (tcp-buffer-size)) rem)))))
      (unless (and pool-empty? (buffer-pool-fixed?))
        ((tcp-buf-queue) 'add! bv))
      (close-output-port op)
      (close-input-port ip)))

  (define (get-proxy header)
    (let ([matches (pregexp-match (host-regex) header)])
      (and (= (length matches) 2)
           (let* ([host-port (decrypt-host (string->utf8 (cadr matches)))]
                  [host-and-port (pregexp-split ":" (utf8->string host-port))])
             (and (>= (length host-and-port) 2)
                  (let* ([port (cadr host-and-port)])
                    (vector (car host-and-port) (substring port 0 (- (string-length port) 1)))))))))

  )
