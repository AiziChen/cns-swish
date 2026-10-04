#!chezscheme
(library (udp-tools)
  (export
   open-udp-socket
   udp-sendto!
   udp-recvfrom!
   close-udp-socket
   AF_INET
   AF_INET6)
  (import (chezscheme))

  ;; --------------------------------------------------------------------
  ;; 1. Platform & Constant Detection
  ;; --------------------------------------------------------------------
  (define libc (load-shared-object #f))

  (define c-socket     (foreign-procedure "socket" (int int int) int))
  (define c-close      (foreign-procedure "close" (int) int))
  (define c-sendto     (foreign-procedure "sendto" (int u8* size_t int u8* int) ssize_t))
  (define c-recvfrom   (foreign-procedure "recvfrom" (int u8* size_t int u8* u8*) ssize_t))
  (define c-setsockopt (foreign-procedure "setsockopt" (int int int u8* int) int))

  ;; Detect OS
  (define macos?
    (memq (machine-type) '(a6osx ta6osx i3osx ti3osx)))

  (define AF_INET     2)
  (define AF_INET6    (if macos? 30 10))
  (define SOCK_DGRAM  2)
  (define SOL_SOCKET  (if macos? #xffff 1))
  (define SO_RCVTIMEO (if macos? #x1006 20))

  ;; --------------------------------------------------------------------
  ;; 2. Sockaddr Construction
  ;; --------------------------------------------------------------------
  (define (make-sockaddr ip-bv port)
    (let ([ip-len (bytevector-length ip-bv)])
      (cond
       [(= ip-len 4)                    ; IPv4
        (let ([sa (make-bytevector 16 0)])
          (if macos?
              (begin
                (bytevector-u8-set! sa 0 16)       ; sin_len
                (bytevector-u8-set! sa 1 AF_INET)) ; sin_family
              (bytevector-u16-set! sa 0 AF_INET (native-endianness)))
          (bytevector-u16-set! sa 2 port (endianness big))
          (bytevector-copy! ip-bv 0 sa 4 4)
          (values sa 16 AF_INET))]
       [(= ip-len 16)                   ; IPv6
        (let ([sa (make-bytevector 28 0)])
          (if macos?
              (begin
                (bytevector-u8-set! sa 0 28)        ; sin6_len
                (bytevector-u8-set! sa 1 AF_INET6)) ; sin6_family
              (bytevector-u16-set! sa 0 AF_INET6 (native-endianness)))
          (bytevector-u16-set! sa 2 port (endianness big))
          (bytevector-copy! ip-bv 0 sa 8 16)
          (values sa 28 AF_INET6))]
       [else (error 'make-sockaddr "Invalid IP bytevector length" ip-len)])))

  ;; --------------------------------------------------------------------
  ;; 3. Socket Timeout Helper
  ;; --------------------------------------------------------------------
  (define (set-udp-timeout! sock timeout-sec)
    (let ([timeval (make-bytevector (if (= (foreign-sizeof 'void*) 8) 16 8) 0)])
      ;; Set tv_sec
      (if (= (foreign-sizeof 'void*) 8)
          (bytevector-u64-set! timeval 0 timeout-sec (native-endianness))
          (bytevector-u32-set! timeval 0 timeout-sec (native-endianness)))
      (c-setsockopt sock SOL_SOCKET SO_RCVTIMEO timeval (bytevector-length timeval))))

  ;; --------------------------------------------------------------------
  ;; 4. UDP API Procedures
  ;; --------------------------------------------------------------------
  (define (open-udp-socket . family-opt)
    (let* ([family (if (null? family-opt) AF_INET (car family-opt))]
           [fd (c-socket family SOCK_DGRAM 0)])
      (if (< fd 0)
          (error 'open-udp-socket "Failed to create UDP socket")
          (begin
            ;; Set a 30-second receive timeout so recvfrom doesn't block forever
            (set-udp-timeout! fd 30)
            fd))))

  (define (close-udp-socket sock)
    (when (and (number? sock) (>= sock 0))
      (c-close sock)))

  (define (udp-sendto! sock ip-bv port bv offset len)
    (let-values ([(sa sa-len domain) (make-sockaddr ip-bv port)])
      (let ([payload (if (and (= offset 0) (= len (bytevector-length bv)))
                         bv
                         (let ([tmp (make-bytevector len)])
                           (bytevector-copy! bv offset tmp 0 len)
                           tmp))])
        (let ([res (c-sendto sock payload len 0 sa sa-len)])
          (if (< res 0)
              (error 'udp-sendto! "sendto system call failed")
              res)))))

  (define (udp-recvfrom! sock bv offset len)
    (let ([sa (make-bytevector 128 0)]
          [sa-len-bv (make-bytevector 4 0)]
          [recv-buf (make-bytevector len 0)])
      (bytevector-u32-set! sa-len-bv 0 128 (native-endianness))
      (let ([res (c-recvfrom sock recv-buf len 0 sa sa-len-bv)])
        (if (< res 0)
            (error 'udp-recvfrom! "recvfrom timed out or failed")
            (begin
              (bytevector-copy! recv-buf 0 bv offset res)
              (let ([family (if macos?
                                (bytevector-u8-ref sa 1)
                                (bytevector-u16-ref sa 0 (native-endianness)))])
                (cond
                 [(= family AF_INET)
                  (let ([src-ip (make-bytevector 4)]
                        [src-port (bytevector-u16-ref sa 2 (endianness big))])
                    (bytevector-copy! sa 4 src-ip 0 4)
                    (values res src-ip src-port))]
                 [(= family AF_INET6)
                  (let ([src-ip (make-bytevector 16)]
                        [src-port (bytevector-u16-ref sa 2 (endianness big))])
                    (bytevector-copy! sa 8 src-ip 0 16)
                    (values res src-ip src-port))]
                 [else
                  (values res (make-bytevector 4 0) 0)])))))))

  )
