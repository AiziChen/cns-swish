#!chezscheme
(library (tools)
  (export
   bytevector-u8-index
   contains
   fstarts-with?
   make-bufpool
   make-queue
   subbytevector)
  (import
   (chezscheme)
   (swish queue))


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
        (or (= i c2len)
            (and (equal? (ref-p c1 (+ i c1-start)) (ref-p c2 i))
                 (lp (+ i 1)))))]))

  (define (contains ref-p c1 c2 c1len c2len)
    (let lp ([i 0])
      (and (= i c1len)
           (or (fstarts-with? ref-p c1 i c2 c2len)
               (lp (+ i 1))))))

  (define (pmake-list p n)
    (cond
     [(= n 0) '()]
     [else
      (cons (p) (pmake-list p (-1+ n)))]))

  (define (make-queue)
    (let ([q queue:empty])
      (lambda (act . arg)
        (cond
         [(eq? act 'add-front!)
          (when (= (length arg) 1)
            (set! q (queue:add-front (car arg) q)))]
         [(eq? act 'add!)
          (when (= (length arg) 1)
            (set! q (queue:add (car arg) q)))]
         [(eq? act 'get) q]
         [(eq? act 'drop!)
          (let ([elt (cadr q)])
            (set! q (queue:drop q))
            elt)]
         [(eq? act 'empty!)
          (set! q queue:empty)]
         [(eq? act 'empty?)
          (queue:empty? q)]
         [(eq? act 'set!)
          (when (and (= (length arg) 1)
                     (pair? (car arg)))
            (set! q (car arg)))]
         [else #f]))))

  (define (make-bufpool buf-size queue-size)
    `(,(pmake-list
        (lambda () (make-bytevector buf-size))
        (-1+ queue-size))
      ,(make-bytevector buf-size)))
  )


#!eof mats
(import (tools))

(isolate-mat tools-test ()
  (define test-string1 "hello, world")
  ;; `fstarts-with?` test
  (let ([s "hello, world"]
        [bv #vu8(33 52 3)])
    (printf "fstarts-with? test #t result1 : ~a~%" (fstarts-with? string-ref s "hello" 5))
    (printf "fstarts-with? test #t result2 : ~a~%" (fstarts-with? bytevector-u8-ref bv #vu8(33 52) 2)))
  ;; `contains` test
  (let ([s "hello, world"])
    (printf "contains test #t result: ~a~%" (contains string-ref s ", wor" (string-length s) (string-length ", wor")))
    (printf "contains test #f result: ~a~%" (contains string-ref s "qwor" (string-length s) (string-length "qwor"))))
  ;; bytevector-u8-index test
  (let ([s (string->utf8 "hello, world")])
    (printf "bytevector-u8-index test result: ~a~%"
      (bytevector-u8-index s (string->utf8 "wor")))
    (printf "bytevector-u8-index test result: ~a~%"
      (bytevector-u8-index s s)))
  )

