#| Solutions to selected exercises from Structure and Interpretation of Computer Programs
 | by Abelson and Sussman, Chapter 3.3. Last updated 13 February 2019 by Marcel Goh
 |#

; Exercise 3.17/3.18
(define make-seen-table
  (let ((seen '()))
    (lambda (query)
      (if (memq query seen)
          #t
          (begin (set! seen (cons query seen))
                 #f)))))
(define (count-pairs x)
  (define seen? make-seen-table)
  (if (not (pair? x))
      0
      (if (seen? x)
          0
          (+ (count-pairs (car x))
             (count-pairs (cdr x))
             1))))
(define (naive-cycle? x)
  (define seen? make-seen-table)
  (if (null? x)
      #f
      (if (seen? x)
          #t
          (naive-cycle? (cdr x)))))

; Exercise 3.19
(define (fast-cycle? x)
  (define (iter sg dbl)
    (cond ((eq? sg dbl)
           #t)
          ((null? dbl)
           #f)
          ((null? (cdr dbl))
           #f)
          (else
           (iter (cdr sg) (cddr dbl)))))
  (cond ((null? x)
         #f)
        ((null? (cdr x))
         #f)
        (else
         (iter (cdr x) (cddr x)))))

; Exercise 3.21/3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (dispatch m)
      (cond ((eq? m 'front-queue)
             (car front-ptr))
            ((eq? m 'empty-queue?)
             (null? front-ptr))
            ((eq? m 'insert-queue!)
             (lambda (item)
               (let ((new-item (cons item '())))
                 (cond ((null? front-ptr)
                        (set! front-ptr new-item)
                        (set! rear-ptr new-item))
                       (else
                        (set-cdr! rear-ptr new-item)
                        (set! rear-ptr new-item))))))
            ((eq? m 'delete-queue!)
             (let ((ret-val (car front-ptr)))
               (set! front-ptr (cdr front-ptr))
               ret-val))
            ((eq? m 'print-queue)
             (display front-ptr))
            (else
             (error "Unknown tag: DISPATCH" m))))
    dispatch))
(define (empty-queue? queue)
  (queue 'empty-queue?))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "Empty queue: FRONT-QUEUE" queue)
      (queue 'front-queue)))
(define (insert-queue! queue item)
  ((queue 'insert-queue!) item)
  queue)
(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "Empty queue: DELETE-QUEUE!" queue)
      (queue 'delete-queue!)))
; Modified Exercise 3.21 to work with local-state version of queue
(define (print-queue queue)
  (queue 'print-queue)
  queue)

; Exercise 3.23
; Helper doubly-linked list implementation
(define (make-node item before after)
  (cons item (cons before after)))
(define (item-node node)
  (car node))
(define (before-node node)
  (cadr node))
(define (after-node node)
  (cddr node))
(define (set-before-node! node new)
  (set-car! (cdr node) new))
(define (set-after-node! node new)
  (set-cdr! (cdr node) new))
; Deque procedures
(define (make-deque) (cons '() '()))
(define (empty-deque? d) (null? (car d)))
(define (front-deque d)
  (if (empty-deque? d)
      (error "Empty deque: FRONT-DEQUE" d)
      (caar d)))
(define (rear-deque d)
  (if (empty-deque? d)
      (error "Empty deque: REAR-DEQUE" d)
      (cadr d)))
(define (front-insert-deque! deque item)
  (let ((new-item (make-node item '() '())))
    (if (empty-deque? deque)
        (begin (set-car! deque new-item)
               (set-cdr! deque new-item))
        (begin (set-after-node! new-item (car deque))
               (set-car! deque new-item)))))
(define (rear-insert-deque! deque item)
  (let ((new-item (make-node item '() ())))
    (if (empty-deque? deque)
        (begin (set-car! deque new-item)
               (set-cdr! deque new-item))
        (begin (set-after-node! (cdr deque) new-item)
               (set-before-node! new-item (cdr deque))
               (set-cdr! deque new-item)))))
(define (front-delete-deque! deque)
  (if (empty-deque? deque)
      (error "Empty deque: FRONT-DELETE-DEQUE!" deque)
      (set-car! deque (after-node (car deque)))))
(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
      (error "Empty deque: REAR-DELETE-DEQUE!" deque)
      (set-cdr! deque (before-node (cdr deque)))))

; Exercise 3.24
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key)
      (define (iter r)
        (cond ((null? r) false)
              ((same-key? key (caar r)) (car r))
              (else (iter (cdr r)))))
      (let ((records (cdr local-table)))
        (iter records)))
    (define (lookup key)
      (let ((record (assoc key)))
        (if record
            (cdr record)
            false)))
    (define (insert! key value)
      (let ((record (assoc key)))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value)
                            (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

