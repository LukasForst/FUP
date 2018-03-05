
(define (mz)
'(
(w w w w w w)
(w 0 w 0 w w)
(w 0 w 0 0 w)
(w 0 0 0 w w)
(w w w w w w)))

(define (stat) (list (mz) `(1 1) `south))

;state - (<maze> (<coordinate-x> <coordinate-y>) <orientation>)
(define (maze-idx) 0)
(define (coord-idx) 1)
(define (orient-idx) 2)
(define (value-on-nth-element li nth-ele) (if (= nth-ele 0) (car li) (value-on-nth-element (cdr li) (- nth-ele 1))))
(define (get-maze state) (value-on-nth-element state (maze-idx)))
(define (get-curr-position state) (value-on-nth-element state (coord-idx)))
(define (get-curr-x state) (value-on-nth-element (value-on-nth-element state (coord-idx)) 0))
(define (get-curr-y state) (value-on-nth-element (value-on-nth-element state (coord-idx)) 1))
(define (get-orient state) (value-on-nth-element state (orient-idx)))
(define (north? state) (if (string=? "north" (symbol->string (get-orient state))) #t #f))
(define (south? state) (if (string=? "south" (symbol->string (get-orient state))) #t #f))
(define (west? state) (if (string=? "west" (symbol->string (get-orient state))) #t #f))
(define (east? state) (if (string=? "east" (symbol->string (get-orient state))) #t #f))
(define (to-list a) (if (list? a) a (list a)))
(define (merge li1 li2) (append (to-list li1) (to-list li2)))

(define (value-xy 2d-arr x y)
  (let ((xs (value-on-nth-element 2d-arr x)))
        (value-on-nth-element xs y)))

(define (get-curr-val state) (value-xy (get-maze state) (get-curr-x state) (get-curr-y state)))

(define (set-x li x val)
  (define (_set-x li x val x-li)
    (cond
      ((null? li) `())
      ((= x 0) (append (append x-li (list val)) (cdr li)))
      (#t (_set-x (cdr li) (- x 1) val (append x-li (list (car li)))))
    ))
  (_set-x li x val `()))

(define (set-xy 2d-arr x y val) (set-x 2d-arr x (set-x (value-on-nth-element 2d-arr x) y val)))

(define (put-mark state)
  (let* ((x (get-curr-x state))
        (y (get-curr-y state))
        (val (value-xy (get-maze state) x y)))
    (if (number? val) (if (< val 0) state (set-x state (maze-idx) (set-xy (get-maze state) x y (+ (value-xy (get-maze state) x y) 1)))) state)))

(define (get-mark state)
  (let* ((x (get-curr-x state))
        (y (get-curr-y state))
        (val (value-xy (get-maze state) x y)))
    (if (number? val) (if (> val 0) (set-x state (maze-idx) (set-xy (get-maze state) x y (- (value-xy (get-maze state) x y) 1))) state) state)))

(define (get-front state)
  (let ((head (get-orient state)))
    (cond
      ((west? state) (value-xy (get-maze state) (get-curr-x state) (- (get-curr-y state) 1)))
      ((east? state) (value-xy (get-maze state) (get-curr-x state) (+ (get-curr-y state) 1)))
      ((north? state) (value-xy (get-maze state) (- (get-curr-x state) 1) (get-curr-y state)))
      ((south? state) (value-xy (get-maze state) (+ (get-curr-x state) 1) (get-curr-y state))))))
  
(define (wall? state) (if (eqv? (get-front state) `w) #t #f))
(define (mark? state) (let ((val (get-curr-val state))) (if (number? val) (if (> val 0) #t #f) #f)))

(define (make-step state)
  (if (wall? state) state
       (cond
          ((west? state) (set-x state (coord-idx) (set-x (get-curr-position state) 1 (- (get-curr-y state) 1))))
          ((east? state) (set-x state (coord-idx) (set-x (get-curr-position state) 1 (+ (get-curr-y state) 1))))
          ((north? state) (set-x state (coord-idx) (set-x (get-curr-position state) 0 (- (get-curr-x state) 1))))
          ((south? state) (set-x state (coord-idx)(set-x (get-curr-position state) 0 (+ (get-curr-x state) 1)))))))

(define (turn-left state)
   (cond
      ((west? state) (set-x state (orient-idx) `south))
      ((east? state) (set-x state (orient-idx) `north))
      ((north? state) (set-x state (orient-idx) `west))
      ((south? state) (set-x state (orient-idx) `east))))

(define (turn-right state) (turn-left(turn-left(turn-left state))))



(define (simulate state expr program limit)
  (simulate-next expr `() state program limit))


(define (simulate-next next-steps previous-steps state procedures limit)
  (if (null? next-steps) (list previous-steps state)
      (let ((next (car next-steps)))
        (cond
          ((eqv? next `step) (simulate-next (cdr next-steps) (merge previous-steps next) (make-step state) procedures limit))
          ((eqv? next `get-mark) (simulate-next (cdr next-steps) (merge previous-steps next) (get-mark state) procedures limit))
          ((eqv? next `put-mark) (simulate-next (cdr next-steps) (merge previous-steps next) (put-mark state) procedures limit) )
          ((eqv? next `turn-left) (simulate-next (cdr next-steps) (merge previous-steps next) (turn-left state) procedures limit))
          (#t (display "err"));evaluate procedure
        ))))

(stat)
(simulate (stat) `(step put-mark step put-mark put-mark put-mark turn-left turn-left turn-left turn-left get-mark) `() 1)


