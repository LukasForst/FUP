#lang racket
(require racket/trace)


;state - (<maze> (<coordinate-x> <coordinate-y>) <orientation>)
(define (maze-idx) 0)
(define (coord-idx) 1)
(define (orient-idx) 2)
(define (value-on-nth-element li nth-ele) (if (= nth-ele 0) (car li) (value-on-nth-element (cdr li) (- nth-ele 1))))
(define (get-maze-state state) (value-on-nth-element state (maze-idx)))
(define (get-curr-position state) (value-on-nth-element state (coord-idx)))
(define (get-curr-x state) (value-on-nth-element (value-on-nth-element state (coord-idx)) 0))
(define (get-curr-y state) (value-on-nth-element (value-on-nth-element state (coord-idx)) 1))
(define (get-orient state) (value-on-nth-element state (orient-idx)))
(define (north? state) (eqv? (get-orient state) `north))
(define (south? state) (eqv? (get-orient state) `south))
(define (west? state) (eqv? (get-orient state) `west))
(define (east? state) (eqv? (get-orient state) `east))
(define (to-list a) (if (list? a) a (list a)))
(define (merge li1 li2) (append (to-list li1) (to-list li2)))

(define (value-xy 2d-arr x y)
  (let ((xs (value-on-nth-element 2d-arr y)))
        (value-on-nth-element xs x)))

(define (get-curr-val state) (value-xy (get-maze-state state) (get-curr-x state) (get-curr-y state)))

(define (set-x li x val)
  (define (_set-x li x val x-li)
    (cond
      ((null? li) `())
      ((= x 0) (append (append x-li (list val)) (cdr li)))
      (#t (_set-x (cdr li) (- x 1) val (append x-li (list (car li)))))
    ))
  (_set-x li x val `()))

(define (set-xy 2d-arr x y val) (set-x 2d-arr y (set-x (value-on-nth-element 2d-arr y) x val)))

(define (put-mark state)
  (set-x state (maze-idx) (set-xy (get-maze-state state) (get-curr-x state) (get-curr-y state)  (+ (value-xy (get-maze-state state) (get-curr-x state) (get-curr-y state)) 1))))

(define (get-mark state)
  (set-x state (maze-idx) (set-xy (get-maze-state state) (get-curr-x state) (get-curr-y state) (- (value-xy (get-maze-state state)  (get-curr-x state) (get-curr-y state)) 1))))

(define (get-front state)
  (let ((head (get-orient state)))
    (cond
      ((north? state) (value-xy (get-maze-state state) (get-curr-x state) (- (get-curr-y state) 1)))
      ((south? state) (value-xy (get-maze-state state) (get-curr-x state) (+ (get-curr-y state) 1)))
      ((west? state) (value-xy (get-maze-state state) (- (get-curr-x state) 1) (get-curr-y state)))
      ((east? state) (value-xy (get-maze-state state) (+ (get-curr-x state) 1) (get-curr-y state))))))
  
(define (wall? state) (eqv? (get-front state) `w))
(define (mark? state) (> (get-curr-val state) 0))

(define (make-step state)
  (if (wall? state) state
       (cond
          ((west? state) (set-x state (coord-idx) (set-x (get-curr-position state) 0 (- (get-curr-x state) 1))))
          ((east? state) (set-x state (coord-idx) (set-x (get-curr-position state) 0 (+ (get-curr-x state) 1))))
          ((north? state) (set-x state (coord-idx) (set-x (get-curr-position state) 1 (- (get-curr-y state) 1))))
          ((south? state) (set-x state (coord-idx)(set-x (get-curr-position state) 1 (+ (get-curr-y state) 1)))))))


(define (turn-left state)
   (cond
      ((west? state) (set-x state (orient-idx) `south))
      ((east? state) (set-x state (orient-idx) `north))
      ((north? state) (set-x state (orient-idx) `west))
      ((south? state) (set-x state (orient-idx) `east))))


(define (simulate state expr program limit)
  (simulate-next expr `() state program limit))

(define (get-procedure-steps program procedure-name)
  (if(eqv? procedure-name (cadr (car program))) (caddr (car program)) (get-procedure-steps (cdr program) procedure-name)))


(define (simulate-next next-steps previous-steps state program limit)
  (if (null? next-steps) (list previous-steps state)
      (let ((next (car (to-list next-steps))))
        (cond
          ((eqv? next `step) (if (wall? state) (list previous-steps state) (simulate-next (cdr (to-list next-steps)) (merge previous-steps next) (make-step state) program limit)))
          ((eqv? next `get-mark) (if (mark? state) (simulate-next (cdr (to-list next-steps)) (merge previous-steps next) (get-mark state) program limit) (list previous-steps state)))
          ((eqv? next `put-mark) (simulate-next (cdr (to-list next-steps)) (merge previous-steps next) (put-mark state) program limit) )
          ((eqv? next `turn-left) (simulate-next (cdr (to-list next-steps)) (merge previous-steps next) (turn-left state) program limit))
          (#t (procedure-call (merge (get-procedure-steps program next) `end) (cdr (to-list next-steps))  previous-steps state program (- limit 1)))
          (#t (display "err"));evaluate procedure
        ))))

(define (procedure-call procedure-steps next-steps previous-steps state program limit)
  (cond
    ((> 0 limit) (list previous-steps state))
    ((null? procedure-steps) (simulate-next next-steps previous-steps state program limit))
    (#t (let ((next (car procedure-steps)))
        (cond
          ((list? next)
           (if (eqv? (car next) `if)
           (let ((condi (cadr next)))
             (cond
               ((eqv? condi `north?)
                (if (north? state)
                    (procedure-call (merge (car (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit);true
                    (procedure-call (merge (cadr (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit)));else
               ((eqv? condi `mark?)
                (if (mark? state)
                    (procedure-call (merge (car (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit);true
                    (procedure-call (merge (cadr (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit)));else);if north then send this program
               ((eqv? condi `wall?)
                (if (wall? state)
                    (procedure-call (merge (car (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit);true
                    (procedure-call (merge (cadr (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit)));else);if north then send this program
               (#t (display "error")); return error
             )) (display next))) ;error?

          ((eqv? next `if)
           (let ((condi (cadr procedure-steps)))
             (cond
               ((eqv? condi `north?)
                (if (north? state)
                    (procedure-call (merge (car (cdr (cdr procedure-steps))) (cddddr procedure-steps)) next-steps previous-steps state program limit);true
                    (procedure-call (merge (cadr (cdr (cdr procedure-steps))) (cddddr procedure-steps)) next-steps previous-steps state program limit)));else
               ((eqv? condi `mark?)
                (if (mark? state)
                    (procedure-call (merge (car (cdr (cdr procedure-steps))) (cddddr procedure-steps)) next-steps previous-steps state program limit);true
                    (procedure-call (merge (cadr (cdr (cdr procedure-steps)))(cddddr procedure-steps)) next-steps previous-steps state program limit)));else);if north then send this program
               ((eqv? condi `wall?)
                (if (wall? state)
                    (procedure-call (merge (car (cdr (cdr procedure-steps))) (cddddr procedure-steps)) next-steps previous-steps state program limit);true
                    (procedure-call (merge (cadr (cdr (cdr procedure-steps))) (cddddr procedure-steps)) next-steps previous-steps state program limit)));else);if north then send this program
               (#t (display "error")); return error
             ))) ;error?
          
          ((eqv? next `end) (procedure-call (cdr procedure-steps) next-steps previous-steps state program (+ limit 1)))
          ((eqv? next `step)(if (wall? state) (list previous-steps state) (procedure-call (cdr procedure-steps) next-steps (merge previous-steps next) (make-step state) program limit)))
          ((eqv? next `get-mark) (if (mark? state) (procedure-call (cdr procedure-steps) next-steps (merge previous-steps next) (get-mark state) program limit) (list previous-steps state)))
          ((eqv? next `put-mark) (procedure-call (cdr procedure-steps) next-steps (merge previous-steps next) (put-mark state) program limit))
          ((eqv? next `turn-left) (procedure-call (cdr procedure-steps) next-steps (merge previous-steps next) (turn-left state) program limit))
          (#t (procedure-call (merge (merge (get-procedure-steps program next) `end) (cdr procedure-steps)) next-steps previous-steps state program (- limit 1)))
        )))
    ))

#|
<prgs> -- is a list of programs for the robot (as in Task 1). Each program includes a procedure named "start", which is the initial expression for the simulation;
<pairs> -- is a list of pairs of states, including the position and the orientation of the robot;
<threshold> -- is lower bounds on the quality of the program in order to appear in the output;
<stack_size> -- is the limit on the robot simulator stack size (see Task 1).
|#

(define (evaluate prgs pairs threshold stack-size) (sort-results (evaluate-programs `() prgs pairs threshold stack-size)))
;(trace evaluate)

;returns (<value> <program>) where values - `(Manhattan distance, Robot configuration distance, The length of the program, Number of steps)
(define (evaluate-programs result-data programs pairs threshold stack-size)
  (if (null? programs) result-data
      (let ((curr-program (car programs))
            (curr-threshold (car threshold)))
         (evaluate-programs (append result-data (list (list (evaluate-pairs `(0 0 0 0) curr-program pairs curr-threshold stack-size) curr-program))) (cdr programs) pairs (cdr threshold) stack-size)
      )
  )
)
;(trace evaluate-programs)

;current state is current values
;returns <values> where values - `(Manhattan distance, Robot configuration distance, The length of the program, Number of steps) for one particular program
(define (evaluate-pairs current-state program pairs threshold stack-size)
  (if (null? pairs) current-state
      (let ((current-pair (car pairs)))
        (let ((current-todo (cadr current-pair))
              (current-expected (car current-pair)))
          (let ((computed (simulate current-todo `start program stack-size))) ;;state expression program limit
            (evaluate-pairs (merge-states (evaluate-computed computed current-expected threshold) current-state) program (cdr pairs) threshold stack-size)
          )
        )
      )
  )
)

;evaluates computed data `(Manhattan distance, Robot configuration distance, The length of the program, Number of steps)
;todo
(define (evaluate-computed computed-state expected-state threshold) `(1 1 1 1))

(define (merge-states state1 state2) (map + state1 state2))

;todo sort results from evaluate-one-pair
(define (sort-results data) data)


;(trace evaluate-computed)
;(trace simulate)




(define prgs
'(
   ( 
      (procedure start
         (turn-right (if wall? (turn-left 
             (if wall? (turn-left (if wall? turn-left step)) step)) step)
                 put-mark start )
      )   
      (procedure turn-right (turn-left turn-left turn-left))
  )
  (
      (procedure start  (put-mark (if wall? turn-left step) start))
  )
  (
      (procedure start (step step step put-mark))
  )
)
)

(define pairs
'(
  (
   (((w w w w w w) 
     (w 0 w 0 w w) 
     (w 1 w 0 0 w) 
     (w 1 0 0 w w) 
     (w w w w w w)) 
     (1 3) south)

   (((w w w w w w) 
     (w 0 w 0 w w) 
     (w 0 w 0 0 w) 
     (w 0 0 0 w w) 
     (w w w w w w)) 
     (1 1) north)
   )
   (
   (((w w w w w w) 
     (w 0 w 0 w w) 
     (w 0 w 2 0 w) 
     (w 1 3 0 w w) 
     (w w w w w w)) 
     (3 3) north)

   (((w w w w w w) 
     (w 0 w 0 w w) 
     (w 0 w 0 0 w) 
     (w 0 0 0 w w) 
     (w w w w w w)) 
     (1 1) north)
  ))
 )

(evaluate prgs pairs '(20 20 20 20) 5)