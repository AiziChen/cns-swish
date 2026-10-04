#!chezscheme
(library (udp-tools)
  (export
   open-udp-socket
   udp-sendto!
   udp-recvfrom!
   close-udp-socket)
  (import (chezscheme))

  ;; --------------------------------------------------------------------
  ;; 1. C Foreign Library & Function Bindings
  ;; --------------------------------------------------------------------
  ;; Load default C library symbols (libc)
  (define libc (load-shared-object #f))

  (define c-socket   (foreign-procedure "socket" (int int int) int))
  (define c-close    (foreign-procedure "close" (int) int))
  (define c-sendto   (foreign-procedure "sendto" (int u8* size_t int u8* int) ssize_t))
  (define c-recvfrom (foreign-procedure "recvfrom" (int u8* size_t int u8* u8*) ssize_t))

  ;; POSIX Constants (Linux default values)
  (define AF_INET     2)
  (define AF_INET6    10) ; Note: standard Linux AF_INET6 is 10 (macOS is 30)
  (define SOCK_DGRAM  2)

  ;; --------------------------------------------------------------------
  ;; 2. Sockaddr Helper Procedures
  ;; --------------------------------------------------------------------
  (define (make-sockaddr ip-bv port)
    (let ([ip-len (bytevector-length ip-bv)])
      (cond
       ;; IPv4 sockaddr_in (16 bytes)
       [(= ip-len 4)
        (let ([sa (make-bytevector 16 0)])
          (bytevector-u16-set! sa 0 AF_INET (native-endianness))
          (bytevector-u16-set! sa 2 port (endianness big))
          (bytevector-copy! ip-bv 0 sa 4 4)
          (values sa 16))]
       ;; IPv6 sockaddr_in6 (28 bytes)
       [(= ip-len 16)
        (let ([sa (make-bytevector 28 0)])
          (bytevector-u16-set! sa 0 AF_INET6 (native-endianness))
          (bytevector-u16-set! sa 2 port (endianness big))
          (bytevector-copy! ip-bv 0 sa 8 16)
          (values sa 28))]
       [else (error 'make-sockaddr "Invalid IP bytevector length" ip-len)])))

  ;; --------------------------------------------------------------------
  ;; 3. Public UDP API Procedures
  ;; --------------------------------------------------------------------

  ;; Creates a unbound UDP socket file descriptor
  (define (open-udp-socket)
    (let ([fd (c-socket AF_INET SOCK_DGRAM 0)])
      (if (< fd 0)
          (error 'open-udp-socket "Failed to create UDP socket")
          fd)))

  ;; Closes the socket descriptor
  (define (close-udp-socket sock)
    (when (and (number? sock) (>= sock 0))
      (c-close sock)))

  ;; Sends data to target IP (4-byte or 16-byte bytevector) and Port
  (define (udp-sendto! sock ip-bv port bv offset len)
    (let-values ([(sa sa-len) (make-sockaddr ip-bv port)])
      (let ([payload (if (and (= offset 0) (= len (bytevector-length bv)))
                         bv
                         (let ([tmp (make-bytevector len)])
                           (bytevector-copy! bv offset tmp 0 len)
                           tmp))])
        (let ([res (c-sendto sock payload len 0 sa sa-len)])
          (if (< res 0)
              (error 'udp-sendto! "sendto system call failed")
              res)))))

  ;; Receives data from socket into bv at offset, returning 3 values:
  ;; (values bytes-read src-ip-bytevector src-port)
  (define (udp-recvfrom! sock bv offset len)
    (let ([sa (make-bytevector 128 0)] ; Buffer for sockaddr_in / sockaddr_in6
          [sa-len-bv (make-bytevector 4 0)] ; Pointer to socklen_t
          [recv-buf (make-bytevector len 0)])
      (bytevector-u32-set! sa-len-bv 0 128 (native-endianness))
      (let ([res (c-recvfrom sock recv-buf len 0 sa sa-len-bv)])
        (if (< res 0)
            (error 'udp-recvfrom! "recvfrom system call failed")
            (begin
              ;; Copy payload into user's buffer at requested offset
              (bytevector-copy! recv-buf 0 bv offset res)
              ;; Extract sender IP & Port from sockaddr structure
              (let ([family (bytevector-u16-ref sa 0 (native-endianness))])
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
