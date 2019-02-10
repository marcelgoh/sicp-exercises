#| Solutions to selected exercises from Structure and Interpretation of Computer Programs
 | by Abelson and Sussman, Chapter 3.1. Last updated 9 February 2019 by Marcel Goh
 |#

; Exercise 3.1
(define (make-accumulator init)
  (let ((acc init))
    (lambda (incr)
      (set! acc (+ acc incr))
      acc)))

; Exercise 3.2
(define (make-monitored f)
  (let ((acc 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls)
             acc)
            ((eq? x 'reset-count)
             (set! acc 0))
            (else
             (set! acc (+ acc 1))
             (f x))))))

; Exercise 3.3/3.4
(define (make-account balance pw)
  (let ((wrong-count 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (wrong _)
      (if (>= wrong-count 7)
          "Calling the cops..."
          (begin (set! wrong-count (+ wrong-count 1))
                 "Incorrect password")))
    (define (dispatch guess m)
      (if (eq? guess pw)
          (begin (set! wrong-count 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request: MAKE-ACCOUNT"
                                    m))))
          wrong))
    dispatch))

; Section 3.1.2 starter code
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

; Exercise 3.5
(define (estimate-integral p x1 y1 x2 y2 trials)
  (define (test)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (p x y)))
  (let ((width (- x2 x1))
        (height (- y2 y1)))
    (if (and (> width 0) (> height 0))
        (* width height (monte-carlo trials test))
        (error "Illegal bounds: ESTIMATE-INTEGRAL"
               (list x1 y1 x2 y2)))))
(define (estimate-pi trials)
  (define (in-unit-circle x y)
    (<= (+ (* x x) (* y y)) 1))
  (estimate-integral in-unit-circle -1.0 -1.0 1.0 1.0 trials))

; Exercise 3.6
; Generate next number using ax + b (mod m)
(define (rand-update x)
  (modulo (+ (* 1103515245 x) 12345) (expt 2 31)))
(define example-rand
  (let ((x 19))
    (lambda ()
      (set! x (rand-update x))
      x)))
(define rand
  ; Default random number if reset is not called
  (let ((curr 19))
    (lambda (tag)
      (cond ((eq? tag 'generate)
             (begin (set! curr (rand-update curr))
                    curr))
            ((eq? tag 'reset)
             (lambda (seed)
               (begin (set! curr seed)
                      curr)))
            (else (error "Tag not recognised: RAND" tag))))))

; Exercise 3.7
(define (make-joint acct old-pw new-pw)
  (lambda (guess m)
    (if (eq? new-pw guess)
        (acct old-pw m)
        (error "Incorrect password to access point" guess))))

; Exercise 3.8
(define f
  (let ((curr 0))
    (lambda (n)
      (if (= n 1)
          (begin (set! curr 1) 0)
          curr))))
