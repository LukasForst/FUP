;#lang racket
;(require racket/trace)


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

(define (flatten x)
(cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))
  )
)

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


(define (simulate state expr program limit threshold)
  (simulate-next expr `() state program limit 0 threshold))

(define (get-procedure-steps program procedure-name)
  (if(eqv? procedure-name (cadr (car program))) (caddr (car program)) (get-procedure-steps (cdr program) procedure-name)))


(define (simulate-next next-steps previous-steps state program limit current-steps threshold)
  (if (or (null? next-steps) (> current-steps threshold)) (list previous-steps state)
      (let ((next (car (to-list next-steps))))
        (cond
          ((eqv? next `step) (if (wall? state) (list previous-steps state) (simulate-next (cdr (to-list next-steps)) (merge previous-steps next) (make-step state) program limit (+ 1 current-steps) threshold)))
          ((eqv? next `get-mark) (if (mark? state) (simulate-next (cdr (to-list next-steps)) (merge previous-steps next) (get-mark state) program limit  (+ 1 current-steps) threshold) (list previous-steps state)))
          ((eqv? next `put-mark) (simulate-next (cdr (to-list next-steps)) (merge previous-steps next) (put-mark state) program limit (+ 1 current-steps) threshold))
          ((eqv? next `turn-left) (simulate-next (cdr (to-list next-steps)) (merge previous-steps next) (turn-left state) program limit (+ 1 current-steps) threshold))
          (#t (procedure-call (merge (get-procedure-steps program next) `end) (cdr (to-list next-steps))  previous-steps state program (- limit 1) (+ 1 current-steps) threshold))
          (#t (display "err"));evaluate procedure
        ))))

(define (procedure-call procedure-steps next-steps previous-steps state program limit current-steps threshold)
  (cond
    ((or (> 0 limit) (> current-steps threshold)) (list previous-steps state))
    ((null? procedure-steps) (simulate-next next-steps previous-steps state program limit current-steps threshold))
    (#t (let ((next (car procedure-steps)))
        (cond
          ((list? next)
           (if (eqv? (car next) `if)
           (let ((condi (cadr next)))
             (cond
               ((eqv? condi `north?)
                (if (north? state)
                    (procedure-call (merge (car (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit (+ 1 current-steps) threshold);true
                    (procedure-call (merge (cadr (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit (+ 1 current-steps) threshold)));else
               ((eqv? condi `mark?)
                (if (mark? state)
                    (procedure-call (merge (car (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit (+ 1 current-steps) threshold);true
                    (procedure-call (merge (cadr (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit (+ 1 current-steps) threshold)));else);if north then send this program
               ((eqv? condi `wall?)
                (if (wall? state)
                    (procedure-call (merge (car (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit (+ 1 current-steps) threshold);true
                    (procedure-call (merge (cadr (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit (+ 1 current-steps) threshold)));else);if north then send this program
               (#t (display "error")); return error
             )) (display next))) ;error?

          ((eqv? next `if)
           (let ((condi (cadr procedure-steps)))
             (cond
               ((eqv? condi `north?)
                (if (north? state)
                    (procedure-call (merge (car (cdr (cdr procedure-steps))) (cddddr procedure-steps)) next-steps previous-steps state program limit (+ 1 current-steps) threshold);true
                    (procedure-call (merge (cadr (cdr (cdr procedure-steps))) (cddddr procedure-steps)) next-steps previous-steps state program limit (+ 1 current-steps) threshold)));else
               ((eqv? condi `mark?)
                (if (mark? state)
                    (procedure-call (merge (car (cdr (cdr procedure-steps))) (cddddr procedure-steps)) next-steps previous-steps state program limit (+ 1 current-steps) threshold);true
                    (procedure-call (merge (cadr (cdr (cdr procedure-steps)))(cddddr procedure-steps)) next-steps previous-steps state program limit (+ 1 current-steps) threshold)));else);if north then send this program
               ((eqv? condi `wall?)
                (if (wall? state)
                    (procedure-call (merge (car (cdr (cdr procedure-steps))) (cddddr procedure-steps)) next-steps previous-steps state program limit (+ 1 current-steps) threshold);true
                    (procedure-call (merge (cadr (cdr (cdr procedure-steps))) (cddddr procedure-steps)) next-steps previous-steps state program limit (+ 1 current-steps) threshold)));else);if north then send this program
               (#t (display "error")); return error
             ))) ;error?
          
          ((eqv? next `end) (procedure-call (cdr procedure-steps) next-steps previous-steps state program (+ limit 1) current-steps threshold))
          ((eqv? next `step)(if (wall? state) (list previous-steps state) (procedure-call (cdr procedure-steps) next-steps (merge previous-steps next) (make-step state) program limit (+ 1 current-steps) threshold)))
          ((eqv? next `get-mark) (if (mark? state) (procedure-call (cdr procedure-steps) next-steps (merge previous-steps next) (get-mark state) program limit (+ 1 current-steps) threshold) (list previous-steps state)))
          ((eqv? next `put-mark) (procedure-call (cdr procedure-steps) next-steps (merge previous-steps next) (put-mark state) program limit (+ 1 current-steps) threshold))
          ((eqv? next `turn-left) (procedure-call (cdr procedure-steps) next-steps (merge previous-steps next) (turn-left state) program limit (+ 1 current-steps) threshold))
          (#t (procedure-call (merge (merge (get-procedure-steps program next) `end) (cdr procedure-steps)) next-steps previous-steps state program (- limit 1) (+ 1 current-steps) threshold))
        )))
    ))

#|
<prgs> -- is a list of programs for the robot (as in Task 1). Each program includes a procedure named "start", which is the initial expression for the simulation;
<pairs> -- is a list of pairs of states, including the position and the orientation of the robot;
<threshold> -- is lower bounds on the quality of the program in order to appear in the output;
<stack_size> -- is the limit on the robot simulator stack size (see Task 1).
|#

(define (evaluate prgs pairs threshold stack-size) (sort-results (evaluate-programs `() prgs pairs threshold stack-size)))

;returns (<value> <program>) where values - `(Manhattan distance, Robot configuration distance, The length of the program, Number of steps)
(define (evaluate-programs result-data programs pairs threshold stack-size)
  (if (null? programs) result-data
      (let ((curr-program (car programs)))
        (let ((evaluated-program (evaluate-pairs `(0 0 (unquote (prog-lenght curr-program)) 0) curr-program pairs (value-on-nth-element threshold 3) stack-size)))
          (if (should-store? threshold evaluated-program)
              (evaluate-programs (append result-data (list (list evaluated-program curr-program))) (cdr programs) pairs threshold stack-size)
              (evaluate-programs result-data (cdr programs) pairs threshold stack-size)                
          )
        )
      )
  )
)

(define (should-store? threshold evaluated-program)
  (and
   (>= (value-on-nth-element threshold 0) (value-on-nth-element evaluated-program 0))
   (>= (value-on-nth-element threshold 1) (value-on-nth-element evaluated-program 1))
   (>= (value-on-nth-element threshold 2) (value-on-nth-element evaluated-program 2))
   (>= (value-on-nth-element threshold 3) (value-on-nth-element evaluated-program 3))
   )
)

(define (prog-lenght program)
  (define (cond-count word count)
    (cond
      ((eqv? `if word) count)
      ((eqv? `procedure word) count)
      (#t (+ count 1))
    )
  )
  
  (define (prog-counter program count)
    (cond
      ((null? program) count)
      (#t (prog-counter (cdr program) (cond-count (car program) count)))
    )
  )
  (prog-counter (flatten program) 0)
)

;current state is current values
;returns <values> where values - `(Manhattan distance, Robot configuration distance, The length of the program, Number of steps) for one particular program
(define (evaluate-pairs current-state program pairs threshold stack-size)
  (if (null? pairs) current-state
      (let ((current-pair (car pairs)))
        (let ((current-todo (car current-pair))
              (current-expected (cadr current-pair)))
          (let ((computed (simulate current-todo `start program stack-size (car (to-list threshold))))) ;;state expression program limit
            (evaluate-pairs (merge-states (evaluate-computed computed current-expected program threshold) current-state) program (cdr pairs) threshold stack-size)
          )
        )
      )
  )
)
(define (merge-states state1 state2) (map + state1 state2))

;evaluates computed data `(Manhattan distance, Robot configuration distance, The length of the program, Number of steps)
(define (evaluate-computed computed-state expected-state program threshold)
  (list (get-manhattan (caadr computed-state) (car expected-state)) (get-robot-config (cdadr computed-state) (cdr expected-state)) 0 (steps-no (car computed-state)))
)

(define (get-manhattan computed-state expected-state)
  (cond
    ((null? expected-state) 0)
    ((list? (car expected-state)) (+ (get-manhattan (car expected-state) (car computed-state)) (get-manhattan (cdr expected-state) (cdr computed-state))))
    ((number? (car expected-state)) (+ (abs (- (car expected-state) (car computed-state))) (get-manhattan (cdr expected-state) (cdr computed-state))))
    (#t (get-manhattan (cdr expected-state) (cdr computed-state)))
  )
)

(define (steps-no computed-state)
  (define (len state count)
    (if (null? state) count (len (cdr state) (+ count 1)))
   )
  (len computed-state 0)
)

(define (get-robot-config computed-state expected-state)
  (define (condi-count counter comp exp)
    (if (number? comp) (+ (abs (- comp exp)) counter)
        (if (eqv? comp exp) counter (+ counter 1))
    )
  )

  (define (executor counter comp exp)
    (if (null? comp) counter (executor (condi-count counter (car comp) (car exp)) (cdr comp) (cdr exp)))
  )

  (executor 0 (flatten computed-state) (flatten expected-state))
)

;todo sort results from evaluate-one-pair
(define (sort-results data) (bubb data))

(define (bubble-up L lambda)
    (if (null? (cdr L))   
        L    
        (if (lambda L)   
            (cons (car L) (bubble-up (cdr L) lambda))   
            (cons (cadr L) (bubble-up (cons (car L) (cddr L)) lambda)))))

(define (bubble-sort-aux length L lambda-condition)    
    (cond ((= length 1) (bubble-up L lambda-condition))   
          (else (bubble-sort-aux (- length 1) (bubble-up L lambda-condition) lambda-condition))))

(define (bubbleH L lambda-condition) 
    (bubble-sort-aux (length L) L lambda-condition))

(define (bubb L)
  (bubbleH (bubbleH (bubbleH (bubbleH L (lambda (L) (<= (cadddr (caar L)) (cadddr (caadr L))))) (lambda (L) (<= (caddr (caar L)) (caddr (caadr L))))) (lambda (L) (<= (cadr (caar L)) (cadr (caadr L))))) (lambda (L) (<= (caaar L) (caaadr L)))))