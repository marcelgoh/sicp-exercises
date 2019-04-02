#| Solutions to exercises in Section 3.3.4 and 3.3.5 in SICP,
 | which simulates digital circuits
 |#

; Provided wire code
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))
(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire newvalue)
  ((wire 'set-signal!) newvalue))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; Delays
(define inverter-delay 2)
(define and-gate-delay 2)
(define or-gate-delay 2)

; NOT-gate code
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

; AND-gate code
(define (logical-and s1 s2)
  ; assuming both s1 and s2 are 0 or 1
  (if (= (+ s1 s2) 2) 1 0))
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
        and-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; Full adder code
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; Exercise 3.28
(define (logical-or s1 s2)
  (if (= (+ s1 s2) 0) 0 1))
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

; Exercise 3.29
; The delay will be (2 * inverter-delay) + and-delay
(define (de-morgan-or a1 a2 output)
  (let ((not-a1 (make-wire))
        (not-a2 (make-wire))
        (not-output (make-wire)))
    (inverter a1 not-a1)
    (inverter a2 not-a2)
    (and-gate not-a1 not-a2 not-output)
    (inverter not-output output)
    'ok))

