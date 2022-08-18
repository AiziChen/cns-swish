#!chezscheme
(library (cipher)
  (export
   xor-cipher!)
  (import
   (chezscheme)
   (tools))
  ;;; data: mutable bytevector
  ;;; secret: password string
  (define xor-cipher!
    (case-lambda
     [(data secret) (xor-cipher! data secret 0)]
     [(data secret subi)
      (let ([data-len (bytevector-length data)]
            [secret-len (string-length secret)])
        (do ([i 0 (+ i 1)])
            ((= i data-len) (remainder (+ subi i) secret-len))
          (let ([rem (remainder (+ subi i) secret-len)]
                [b (bytevector-u8-ref data i)])
            (bytevector-u8-set! data i
              (bitwise-xor
               (bitwise-ior (char->integer (string-ref secret rem)) rem)
               b)))))]))
  )

#!eof mats
(import
 (cipher)
 (tools))

(isolate-mat cipher-test ()
  (define *td* (string->utf8 "hello, world. How are you today?"))
  (define *secret* "quanyec")
  (let ([td *td*])
    (printf "data: ~a~n" (utf8->string td))
    (xor-cipher! td *secret*)
    (xor-cipher! td *secret*)
    (printf "de-data: ~a~n" (utf8->string td)))
  (let ([en-data (base64-decode-bytevector (string->utf8 "SVtbQUVLX0tAUG8="))])
    (xor-cipher! en-data *secret*)
    (printf "~a~n" (utf8->string en-data))))
