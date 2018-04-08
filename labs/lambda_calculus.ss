#lang scheme

(define T (lambda (x) (lambda (y) x)))
(define F (lambda (x) (lambda (y) y)))

(define my-and (lambda (x y) (x y F)))

(define my-or (lambda (x y) (x T y)))



(define my-if (lambda (b x y) (b x y)))

(define Z F)

(define S
  (lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))))

(define (from-scheme-num n)
  (if (= n 0)
      Z
      (S (from-scheme-num (- n 1)))))

(define (to-scheme-num l)
  ((l (lambda (x) (+ 1 x))) 0))

(define my-negation
  (lambda (b) ((b F) T)))

(define is-zero
  (lambda (n) (((n F) my-negation) F)))