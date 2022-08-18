#!chezscheme
(library (tools)
  (export
   bytevector-u8-index
   contains
   fstarts-with?
   subbytevector)
  (import
   (chezscheme))


  (define bytevector-u8-index
    (case-lambda
     [(bv s) (bytevector-u8-index bv 0 s)]
     [(bv index s)
      (define bvlen (bytevector-length bv))
      (define slen (bytevector-length s))
      (let lp ([index index])
        (cond
         [(= index bvlen) #f]
         [(fstarts-with? bytevector-u8-ref bv index s slen) index]
         [else (lp (+ index 1))]))]))

  (define subbytevector
    (case-lambda
     [(bv end) (subbytevector bv 0 end)]
     [(bv start end)
      (u8-list->bytevector
       (let lp ([index start])
         (cond
          [(= index end) '()]
          [else
           (cons (bytevector-u8-ref bv index)
             (lp (+ index 1)))])))]))

  (define fstarts-with?
    (case-lambda
     [(ref-p c1 c2 c2len)
      (fstarts-with? ref-p c1 0 c2 c2len)]
     [(ref-p c1 c1-start c2 c2len)
      (let lp ([i 0])
        (if (= i c2len)
            #t
            (and (equal? (ref-p c1 (+ i c1-start)) (ref-p c2 i))
                 (lp (+ i 1)))))]))

  (define (contains ref-p c1 c2 c1len c2len)
    (let lp ([i 0])
      (if (= i c1len)
          #f
          (or (fstarts-with? ref-p c1 i c2 c2len)
              (lp (+ i 1)))))))


#!eof mats
(import (tools))

(isolate-mat tools-test ()
  (define test-string1 "hello, world")
  ;; `fstarts-with?` test
  (let ([s "hello, world"]
        [bv #vu8(33 52 3)])
    (printf "fstarts-with? test #t result1 : ~a~n" (fstarts-with? string-ref s "hello" 5))
    (printf "fstarts-with? test #t result2 : ~a~n" (fstarts-with? bytevector-u8-ref bv #vu8(33 52) 2)))
  ;; `contains` test
  (let ([s "hello, world"])
    (printf "contains test #t result: ~a~n" (contains string-ref s ", wor" (string-length s) (string-length ", wor")))
    (printf "contains test #f result: ~a~n" (contains string-ref s "qwor" (string-length s) (string-length "qwor"))))
  ;; bytevector-u8-index test
  (let ([s (string->utf8 "hello, world")])
    (printf "bytevector-u8-index test result: ~a~n"
      (bytevector-u8-index s (string->utf8 "wor")))
    (printf "bytevector-u8-index test result: ~a~n"
      (bytevector-u8-index s s)))
  )

