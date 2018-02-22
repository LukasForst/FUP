#lang scheme


(define a 6) ;defining a constant

(define (hw arg) (string-append "hello " arg)) ;defining a function

(if (< 2 1) 1 2) ; First option for if

(cond         ;Second option for if statement 
  ((> 1 2) 5)
  (#t 4)
  )

(define (fact n) ; defining a factorial
  (cond
    ((= n 1) 1)
    ((< n 1) (quote "Error, negative input"))
    (#t (* n(fact (- n 1))))
  )
)

;car for first element, cdr for second element, caddr for third element in the array

(define (last li) ;function to access last element in an array
  (cond
    ((null? (cdr li)) (car li))
    (#t (last (cdr li)))
  )
)
(pair? '(1 2 3)) ;returns #t

(cons 1 2) ;creates a pair, returns: (1 . 2)
(cons 1 '()) ;returns (1)

(list 1 2 3) ;creates a list, returns: (1 2 3)
