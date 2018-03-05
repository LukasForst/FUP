
(define (mz)
'(
(w w w w w w)
(w 0 w 0 w w)
(w 0 w 0 0 w)
(w 0 0 0 w w)
(w w w w w w)))

(define (stat) (list (mz) `(1 1) 'N))

;state - (<maze> (<coordinate-x> <coordinate-y>) <orientation>)
(define (value-on-nth-element li nth-ele) (if (= nth-ele 0) (car li) (value-on-nth-element (cdr li) (- nth-ele 1))))
(define (get-maze state) (value-on-nth-element state 0))
(define (get-curr-x state) (value-on-nth-element (value-on-nth-element state 1) 0))
(define (get-curr-y state) (value-on-nth-element (value-on-nth-element state 1) 1))
(define (get-coord state) (value-on-nth-element state 2))
(define (to-list a) (if (list? a) a (list a)))
(define (merge li1 li2) (append (to-list li1) (to-list li2)))

(define (value-xy maze x y)
  (let ((xs (value-on-nth-element maze x)))
        (value-on-nth-element xs y)))

(define (set-x li x val)
  (define (_set-x li x val x-li)
    (cond
      ((null? li) `())
      ((= x 0) (append (append x-li (list val)) (cdr li)))
      (#t (_set-x (cdr li) (- x 1) val (append x-li (list (car li)))))
    ))
  (_set-x li x val `()))

(define (set-xy maze x y val)
  (set-x maze x (set-x (value-on-nth-element maze x) y val)))

(define (put-mark state)
  (let* ((x (get-curr-x state))
        (y (get-curr-y state))
        (val (value-xy (get-maze state) x y)))
    (if (number? val) (if (= val 0) (set-xy (get-maze state) x y (+ (value-xy (get-maze state) x y) 1)) state) state)))

(define (get-mark state)
  (let* ((x (get-curr-x state))
        (y (get-curr-y state))
        (val (value-xy (get-maze state) x y)))
    (if (number? val) (if (< val 1) (set-xy (get-maze state) x y (- (value-xy (get-maze state) x y) 1)) state) state)))

(put-mark (stat))
(put-mark (stat))

(define (simulate state expr program limit)
  (get-maze state)) ;state - (<maze> (<coordinate-x> <coordinate-y>) <orientation>)

;(value-on-nth-element `(1 2 3) 0)
;(get-coord `(1 (2 3) "a"))