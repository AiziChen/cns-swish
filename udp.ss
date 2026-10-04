#!chezscheme
(library (udp)
  (export
    process-udpsession)
  (import
    (chezscheme)
    (swish imports)
    (common)
    (udp-tools))

  ;; ====================================================================
  ;; 1. Helper: Parse & Forward Packets (TCP Stream -> Remote UDP)
  ;; ====================================================================
  (define (write-to-udp-server! udp-sock bv len)
    ;; Returns the number of bytes successfully consumed (pkgSub),
    ;; or -1 if a protocol/write error occurred.
    (let lp ([pkg-sub 0])
      (cond
       ;; Not enough bytes left to read the 2-byte package length
       [(>= pkg-sub (- len 2)) pkg-sub]
       [else
        (let ([pkg-len (bytevector-u16-ref bv pkg-sub (endianness little))])
          (cond
           ;; Partial packet in buffer or invalid length
           [(or (> (+ pkg-sub 2 pkg-len) len) (< pkg-len 10))
            pkg-sub]
           ;; Validate header bytes [pkgSub+3:pkgSub+5] == 0
           [(not (and (= (bytevector-u8-ref bv (+ pkg-sub 3)) 0)
                      (= (bytevector-u8-ref bv (+ pkg-sub 4)) 0)))
            -1]
           [else
            (let ([atyp (bytevector-u8-ref bv (+ pkg-sub 5))])
              (cond
               ;; IPv4 Target
               [(= atyp 1)
                (if (<= (+ pkg-sub 12) len)
                    (let ([ip (make-bytevector 4)]
                          [port (bytevector-u16-ref bv (+ pkg-sub 10) (endianness big))]
                          [head-len 12])
                      (bytevector-copy! bv (+ pkg-sub 6) ip 0 4)
                      (let* ([payload-start (+ pkg-sub head-len)]
                             [payload-end (+ pkg-sub 2 pkg-len)]
                             [payload-size (- payload-end payload-start)])
                        (match (try (udp-sendto! udp-sock ip port bv payload-start payload-size))
                          [`(catch ,_) -1]
                          [_ (lp payload-end)])))
                    pkg-sub)]
               ;; IPv6 Target
               [(or (= atyp 3) (= atyp 4))
                (if (and (<= (+ pkg-sub 24) len) (>= pkg-len 22))
                    (let ([ip (make-bytevector 16)]
                          [port (bytevector-u16-ref bv (+ pkg-sub 22) (endianness big))]
                          [head-len 24])
                      (bytevector-copy! bv (+ pkg-sub 6) ip 0 16)
                      (let* ([payload-start (+ pkg-sub head-len)]
                             [payload-end (+ pkg-sub 2 pkg-len)]
                             [payload-size (- payload-end payload-start)])
                        (match (try (udp-sendto! udp-sock ip port bv payload-start payload-size))
                          [`(catch ,_) -1]
                          [_ (lp payload-end)])))
                    pkg-sub)]
               [else -1]))]))])))

  ;; ====================================================================
  ;; 2. Client TCP -> Remote UDP Forwarder Loop
  ;; ====================================================================
  (define (tcp->udp-forward ip udp-sock initial-bv)
    (let* ([buf (make-bytevector 65536)]
           [init-len (bytevector-length initial-bv)])
      (bytevector-copy! initial-bv 0 buf 0 init-len)
      (try
       (let lp ([total-len init-len]
                [subi 0])
         (let* ([next-subi (if (> total-len 0) 
                               (decrypt-data! buf 0 total-len subi) 
                               subi)]
                [consumed (write-to-udp-server! udp-sock buf total-len)])
           (cond
            [(= consumed -1)
             (close-udp-socket udp-sock)
             (close-input-port ip)]
            [else
             (let ([rem (- total-len consumed)])
               ;; Shift remaining unparsed bytes to the front of the buffer
               (when (> rem 0)
                 (bytevector-copy! buf consumed buf 0 rem))
               (let ([n (get-bytevector-some! ip buf rem (- (bytevector-length buf) rem))])
                 (unless (eof-object? n)
                   (lp (+ rem n) next-subi))))]))))
      (close-udp-socket udp-sock)
      (close-input-port ip)))

  ;; ====================================================================
  ;; 3. Remote UDP -> Client TCP Forwarder Loop
  ;; ====================================================================
  (define (udp->tcp-forward udp-sock op)
    (let ([bv (make-bytevector 65536)])
      (try
       (let lp ([subi 0])
         ;; Read UDP payload into bv starting at index 24 (reserving header space)
         (let-values ([(payload-len src-ip src-port)
                       (udp-recvfrom! udp-sock bv 24 (- 65536 24))])
           (when (and payload-len (> payload-len 0))
             (let* ([is-ipv4? (= (bytevector-length src-ip) 4)]
                    [ignore-head-len (if is-ipv4? 12 0)])
               (if is-ipv4?
                   ;; Framing IPv4 Header
                   (begin
                     (bytevector-u16-set! bv 12 (+ payload-len 10) (endianness little))
                     (bytevector-u8-set! bv 14 0)
                     (bytevector-u8-set! bv 15 0)
                     (bytevector-u8-set! bv 16 0)
                     (bytevector-u8-set! bv 17 1)
                     (bytevector-copy! src-ip 0 bv 18 4))
                   ;; Framing IPv6 Header
                   (begin
                     (bytevector-u16-set! bv 0 (+ payload-len 22) (endianness little))
                     (bytevector-u8-set! bv 2 0)
                     (bytevector-u8-set! bv 3 0)
                     (bytevector-u8-set! bv 4 0)
                     (bytevector-u8-set! bv 5 3)
                     (bytevector-copy! src-ip 0 bv 6 16)))

               ;; Set Big-Endian Remote Port at bytes 22-23
               (bytevector-u16-set! bv 22 src-port (endianness big))

               (let* ([out-start ignore-head-len]
                      [out-len (- (+ 24 payload-len) ignore-head-len)]
                      [next-subi (encrypt-data! bv out-start out-len subi)])
                 (put-bytevector op bv out-start out-len)
                 (flush-output-port op)
                 (lp next-subi)))))))
      (close-udp-socket udp-sock)
      (close-output-port op)))

  ;; ====================================================================
  ;; 4. Main Entry Point: process-udpsession
  ;; ====================================================================
  (define (process-udpsession ip op initial-bv)
    (match (try (open-udp-socket))
      [`(catch ,reason)
       (printf "Failed to open UDP socket: ~a~%" reason)
       (close-input-port ip)
       (close-output-port op)]
      [#(result ,udp-sock)
       (printf "Start httpUDP session~%")
       ;; Spawn UDP -> TCP forwarder in background
       (spawn&link (lambda () (udp->tcp-forward udp-sock op)))
       ;; Run TCP -> UDP forwarder in main worker process
       (tcp->udp-forward ip udp-sock initial-bv)]))

  )
