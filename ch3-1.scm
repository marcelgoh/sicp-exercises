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
