#lang scheme

(define (pascal n)
  (cond
    ((= n 1) `(1))
    (else (let ((prev (pascal (- n 1))))
            (map + (cons 0 prev) (append prev `(0)))))
     )
  )

(define (product li) (foldr 1 * li))


(define (concat-strings li) (foldr string-append "" li))

(define (my-reverse li)
  (foldl cons (list) li))


(define (my-min li)
  (foldl (lambda (x y) (if (< x y) x y)) (car li) (cdr li)))

(define (dot v1 v2) (foldl + 0 (map * v1 v2)))

(define (mxv matrix vector)
  (map (lambda (row) (dot row vector)) matrix))

(define (transpose matrix)
  (apply map list matrix))

