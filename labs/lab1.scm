(define (discr a b c) (- (* b b) (* 4 (* a c))));discriminant computing

(define (roots a b c)
  (cons (/ (+ (- b) (sqrt (discr a b c))) (* 2 a))
        (/ (- (- b) (sqrt (discr a b c))) (* 2 a))
  )
)

(define (my-even? n)
  (cond
    ((= n 0) #t)
    ((= n 1) #f)
    ((< n 0) (my-even? (+ n 2)))
    ((> n 0) (my-even? (- n 2)))
  )
)

(define (fibonacci n) ;dummy fibonacci 
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fibonacci (- n 2)) (fibonacci (- n 1)))) ;it is possible to write #t instead of else key word
    )
  )

(define (fib-ef n k f0 f1) ;n wanted fib no, f1 is (k)th fib. no., f0 is (k-1)th fib. no.
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    ((= n k) f1)
    (else (fib-ef n (+ k 1) f1 (+ f1 f0)))
  )
)

(define (fib n) (fib-ef n 1 0 1)) ;pretty and fast fibonacci
    
;takes list and returns list of even nums
(define (take-even n) ;n is list of numbers
  (cond
    ((null? n) `())
    ((even? (car n)) (cons (car n) (take-even (cdr n))))
    (else (take-even (cdr n)))
    )
  )
  
(take-even `(1 2 4 3 5 6))