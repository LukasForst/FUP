#lang racket
;#lang r5rs
;(require racket/trace)
;(require rnrs/mutable-pairs-6)

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

(define get-x value-on-nth-element)
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
  (cond
    ((null? next-steps) (list previous-steps state))
    ((> current-steps threshold) (list (merge previous-steps `step) state))
    (#t
      (let ((next (car (to-list next-steps))))
        (cond
          ((eqv? next `step) (if (wall? state) (list previous-steps state) (simulate-next (cdr (to-list next-steps)) (merge previous-steps next) (make-step state) program limit (+ 1 current-steps) threshold)))
          ((eqv? next `get-mark) (if (mark? state) (simulate-next (cdr (to-list next-steps)) (merge previous-steps next) (get-mark state) program limit  (+ 1 current-steps) threshold) (list previous-steps state)))
          ((eqv? next `put-mark) (simulate-next (cdr (to-list next-steps)) (merge previous-steps next) (put-mark state) program limit (+ 1 current-steps) threshold))
          ((eqv? next `turn-left) (simulate-next (cdr (to-list next-steps)) (merge previous-steps next) (turn-left state) program limit (+ 1 current-steps) threshold))
          (#t (procedure-call (merge (get-procedure-steps program next) `end) (cdr (to-list next-steps))  previous-steps state program (- limit 1) current-steps threshold))
          (#t (display "err"));evaluate procedure
        )))))

(define (procedure-call procedure-steps next-steps previous-steps state program limit current-steps threshold)
  (cond
    ((> 0 limit) (list previous-steps state))
    ((> current-steps threshold) (list (merge previous-steps `step) state))
    ((null? procedure-steps) (simulate-next next-steps previous-steps state program limit current-steps threshold))
    (#t (let ((next (car procedure-steps)))
        (cond
          ((list? next)
           (if (eqv? (car next) `if)
           (let ((condi (cadr next)))
             (cond
               ((eqv? condi `north?)
                (if (north? state)
                    (procedure-call (merge (car (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit current-steps threshold);true
                    (procedure-call (merge (cadr (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit current-steps threshold)));else
               ((eqv? condi `mark?)
                (if (mark? state)
                    (procedure-call (merge (car (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit current-steps threshold);true
                    (procedure-call (merge (cadr (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit current-steps threshold)));else);if north then send this program
               ((eqv? condi `wall?)
                (if (wall? state)
                    (procedure-call (merge (car (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit current-steps threshold);true
                    (procedure-call (merge (cadr (cdr (cdr next))) (cdr procedure-steps)) next-steps previous-steps state program limit current-steps threshold)));else);if north then send this program
               (#t (display "error")); return error
             )) (display next))) ;error?

          ((eqv? next `if)
           (let ((condi (cadr procedure-steps)))
             (cond
               ((eqv? condi `north?)
                (if (north? state)
                    (procedure-call (merge (car (cdr (cdr procedure-steps))) (cddddr procedure-steps)) next-steps previous-steps state program limit current-steps threshold);true
                    (procedure-call (merge (cadr (cdr (cdr procedure-steps))) (cddddr procedure-steps)) next-steps previous-steps state program limit current-steps threshold)));else
               ((eqv? condi `mark?)
                (if (mark? state)
                    (procedure-call (merge (car (cdr (cdr procedure-steps))) (cddddr procedure-steps)) next-steps previous-steps state program limit current-steps threshold);true
                    (procedure-call (merge (cadr (cdr (cdr procedure-steps)))(cddddr procedure-steps)) next-steps previous-steps state program limit current-steps threshold)));else);if north then send this program
               ((eqv? condi `wall?)
                (if (wall? state)
                    (procedure-call (merge (car (cdr (cdr procedure-steps))) (cddddr procedure-steps)) next-steps previous-steps state program limit current-steps threshold);true
                    (procedure-call (merge (cadr (cdr (cdr procedure-steps))) (cddddr procedure-steps)) next-steps previous-steps state program limit current-steps threshold)));else);if north then send this program
               (#t (display "error")); return error
             ))) ;error?
          
          ((eqv? next `end) (procedure-call (cdr procedure-steps) next-steps previous-steps state program (+ limit 1) current-steps threshold))
          ((eqv? next `step)(if (wall? state) (list previous-steps state) (procedure-call (cdr procedure-steps) next-steps (merge previous-steps next) (make-step state) program limit (+ 1 current-steps) threshold)))
          ((eqv? next `get-mark) (if (mark? state) (procedure-call (cdr procedure-steps) next-steps (merge previous-steps next) (get-mark state) program limit (+ 1 current-steps) threshold) (list previous-steps state)))
          ((eqv? next `put-mark) (procedure-call (cdr procedure-steps) next-steps (merge previous-steps next) (put-mark state) program limit (+ 1 current-steps) threshold))
          ((eqv? next `turn-left) (procedure-call (cdr procedure-steps) next-steps (merge previous-steps next) (turn-left state) program limit (+ 1 current-steps) threshold))
          (#t (procedure-call (merge (merge (get-procedure-steps program next) `end) (cdr procedure-steps)) next-steps previous-steps state program (- limit 1) current-steps threshold))
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
  (bubbleH
   (bubbleH
    (bubbleH
     (bubbleH L (lambda (L) (<= (cadddr (caar L)) (cadddr (caadr L)))))
     (lambda (L) (<= (caddr (caar L)) (caddr (caadr L)))))
    (lambda (L) (<= (cadr (caar L)) (cadr (caadr L)))))
   (lambda (L) (<= (caaar L) (caaadr L)))))


;--------------------------------------------------------- HW3
(define one-gen-pop 20)

(define (congruential-rng seed)
  (let ((a 16807 #|(expt 7 5)|#)
        (m 2147483647 #|(- (expt 2 31) 1)|#))
    (let ((m-1 (- m 1)))
      (let ((seed (+ (remainder seed m-1) 1)))
        (lambda (b)
          (let ((n (remainder (* a seed) m)))
            (set! seed n)
            (quotient (* (- n 1) b) m-1)))))))
(define random (congruential-rng 12345))

(define init
  '(
    ((procedure start ()))
    ((procedure start (start)))
    ((procedure start (step)))
    ((procedure start (step start)))
    ((procedure start ((if wall? () (step start step)) put-mark)))
    ((procedure start ((if north? (start) ()) turn-left start)))
    ((procedure start (turn-left turn-left turn-left)))
    ((procedure start (put-mark (if mark? (turn-left turn-left) ()) step put-mark)))
    ((procedure start (step step step put-mark)))
    ((procedure start (put-mark (if wall? turn-left step) start)))
    ((procedure start (put-mark)))
    ((procedure start ((if wall? put-mark step))))
    ((procedure start ((if wall? put-mark (step start)))))
    ((procedure start ((if wall? put-mark (step start turn-left turn-left step turn-left turn-left)))))
    ((procedure start ((if wall? put-mark (step start)))))
    ((procedure start (step step step put-mark)))
   ))

(define (selection eval-results)
  (define (selection-rec eval selected selected-size prob)
    (if (null? eval) selected
        (let ((current (car eval))
              (rd (random 100)))
          (cond
            ((> selected-size (/ one-gen-pop 2)) selected)
            ((< selected-size (/ one-gen-pop 4)) (selection-rec (cdr eval) (append selected (list (cadr current))) (+ selected-size 1) (- 5 prob)))
            ((> rd prob) (selection-rec (cdr eval) (append selected (list (cadr current))) (+ selected-size 1) (- 5 prob)))
            (#t (selection-rec (cdr eval) selected selected-size (- 5 prob)))))))
  (selection-rec eval-results `() 0 100))

(define (set-instruction prg idx instruction)
    (set-x prg 0 (set-x (car prg) 2 (set-x (caddar prg) idx instruction))))

(define (append-instruction prg instruction)
    (set-x prg 0 (set-x (car prg) 2 (merge (caddar prg) `step))))

(define (move-instructions)
  (define ins
    `(step turn-left put-mark get-mark))

  (if (= 0 (random 15)) (merge ins `start) ins))

(define (get-random-move)
  (let ((ins (move-instructions)))
    (let ((rd (random (length ins))))
      (get-x ins rd))))

(define if-instructions
  `((if mark? () ()) (if north? () ()) (if wall? () ())))

(define (get-random-if)
  (let ((rd (random 3)))
    (let ((rd-if (get-x if-instructions rd))
          (rd-ins1 (random 10))
          (rd-ins2 (random 10)))
      (set-x (set-x rd-if 2 (if (> rd-ins1 6) (get-random-move) `())) 3 (if (> (+ rd-ins2 rd-ins2) 15) (get-random-move) `())))))

(define (mutation programs)
  (define (append-mut program)
    (append-instruction program (if (= (random 3) 0) (get-random-if) (get-random-move))))

  (define (ins-mut program)
    (set-instruction program (random (length (caddar program))) (if (= (random 3) 0) (get-random-if) (get-random-move))))
        
  (define (mutation-rec programs mutated)
    (if (null? programs) mutated
        (let ((rd (random 10))
              (curr (car programs)))
          (mutation-rec (cdr programs) (append mutated (list (if (or (< (length (caddar curr)) 3) (> rd 8)) (append-mut curr) (ins-mut curr))))))))

  (mutation-rec programs `()))

(define (crossover programs)
  programs)

(define create-initial
  ;(append (crossover (mutation init)) init))
  init)

(define (evolve pairs threshold stack-size)
  (evolve-prg pairs threshold stack-size create-initial '((9999999 9999999 9999999 9999999))))

(define (print-progs prg)
  (if (null? prg) `()
      (begin (display '-----------) (newline) (display (car prg)) (display '-----------) (newline) (print-progs (cdr prg)))))

(define (evolve-prg pairs threshold stack-size programs current-best)
  (let ((result (evaluate programs pairs threshold stack-size)))
    (let ((res-best (car result)))
      (let ((best (car (sort-results (list res-best current-best)))))
        (begin
          (display best)
          (newline)
          (evolve-prg pairs threshold stack-size (perform-evolution (append (list best) result)) best))))))

(define (perform-evolution eval-results)
  (crossover (mutation (selection eval-results))))