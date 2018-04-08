#lang scheme

(define (ints-from n)
  (cons n
        (lambda () (ints-from (+ n 1)))))

(define nat-nums
  (ints-from 1))

(define (first-n n stream)
  (if (= n 0) stream
      (cons (car stream) (first-n (- n 1) ((cdr stream))))))

(define (fibs)
  (define (fibs-hlpr a b)
    (cons a (lambda () (fibs-hlpr b (+ a b)))))
  (fibs-hlpr 0 1))



;stack implementation

(define (make-stack)
  (let ((stack `()))
    
    (define (size) (length stack))
    (define (top) (car stack))
    (define (pop)
      (let ((tp (top)))
        (begin (set! stack (cdr stack)) tp))) ; remove top
    (define (push m)
      (set! stack (append (list m) stack)))
    (define (dispatch m)
      (cond
        ((eqv? m `size) size)
        ((eqv? m `top) top)
        ((eqv? m `pop) pop)
        ((eqv? m `push) push)))
    dispatch))

(define s (make-stack))