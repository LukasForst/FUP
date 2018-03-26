(define (reverse2 li)
  (if (null? li) `()
      (append (reverse2 (cdr li)) (list (car li)))
      )
)

(define (reverse3 li acc);acc = accumulator
  (if (null? li) acc
     (reverse3 (cdr li) (cons (car li) acc)))
)

(define (rev3 li)
  (reverse3 li `()))

(define (flatten li)
  (cond
    ((null? li) li)
    ((list? (car li)) (append (flatten (car li)) (flatten (cdr li))))
    (#t (cons (car li) (flatten (cdr li)))))
  )


(define (dist p1 p2)
  (let ((a (- (car p1) (cadr p2)))
    (b (- (car p2) (cadr p1))))
    (sqrt(+ (* a a) (* b b)))
    ))

(define (filter fun? xs)
  (if (null? xs) `()
      (let ((tmpRes (filter fun? (cdr xs))))
      (if (fun? (car xs)) (cons (car xs) tmpRes) tmpRes))))

(define (remove x xs)
  (filter (lambda (y) (not (= y x))) xs))

(define (removeDuplicates xs)
  (if (null? xs) xs
      (cons (car xs) (removeDuplicates (remove (car xs) (cdr xs))))
  ))

(removeDuplicates `(1 1 2 2 3 4 4 5 5 5))
