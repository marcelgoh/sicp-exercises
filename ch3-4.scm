#| Solutions to selected exercises from Structure and Interpretation of Computer Programs
 | by Abelson and Sussman, Chapter 3.4, which deals with concurrent systems
 |#

; ######################################################################
; Copied from the SICP course webpage, Problem Set 7
; Copyright 1985-1996 MIT, non-commercial use
(define disallow-preempt-current-thread
  (access disallow-preempt-current-thread
	  (->environment '(runtime thread))))

(define allow-preempt-current-thread
  (access allow-preempt-current-thread
	  (->environment '(runtime thread))))

(define (kill-thread thread)
  (let ((event
	 (lambda ()
	   (exit-current-thread 'RIP))))
    (without-interrupts
     (lambda ()
       (case (thread-execution-state thread)
	 ((STOPPED) (restart-thread thread #t event))
	 ((DEAD) unspecific)
	 (else (signal-thread-event thread event)))))))

(define (parallel-execute . thunks)
  (let ((my-threads '()))
    (define (terminator)
      (without-interrupts
       (lambda ()
	 (for-each kill-thread my-threads)
	 (set! my-threads '())
	 unspecific)))
    (without-interrupts
     (lambda ()
       (set! my-threads
	     (map (lambda (thunk)
		    (let ((thread (create-thread #f thunk)))
		      (detach-thread thread)
		      thread))
		  thunks))
       unspecific))
    terminator))
; ######################################################################
; Provided code from book
; Mutex
(define (clear! cell) (set-car! cell #f))
(define (test-and-set! cell)
  (without-interrupts
    (lambda ()
      (if (car cell)
          #t
          (begin (set-car! cell #t)
                 #f)))))
(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
; Serializer
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

; Exercise 3.47
(define (make-semaphore-mutex n)  ; mutex version
  (let ((count n)
        (the-mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (the-mutex 'acquire)
             (if (> count 0)
                 (begin (set! count (- count 1))
                        (the-mutex 'release))
                 (begin (the-mutex 'release)
                        (the-semaphore 'acquire))))
            ((eq? m 'release)
             (the-mutex 'acquire)
             (if (< count n)
                 (set! count (+ count 1)))
             (the-mutex 'release))
            (else (error "Unknown request: MAKE-SEMAPHORE-MUTEX" m))))
    the-semaphore))
(define (make-semaphore n)       ; test-and-set! version
  (let ((count n)
        (cell (list #f)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-semaphore 'acquire)
                 (begin (set! count (- count 1))
                        (clear! cell))))
            ((eq? m 'release)
             (if (test-and-set! cell)
                 (the-semaphore 'release)
                 (begin (if (< count n)
                            (set! count (+ count 1)))
                        (clear! cell))))
            (else (error "Unknown request: MAKE-SEMAPHORE" m))))
    the-semaphore))

; Exercise 3.48
; Modified make-account code
(define (make-account-and-serializer balance id) ; added account id
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'id) id)            ; get account id
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

; New serialized exchange procedure
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< (account1 'id) (account2 'id))
        ((serializer1 (serializer2 exchange)) account1 account2)
        ((serializer2 (serializer1 exchange)) account1 account2))))

