#| Solutions to some exercises from Chapter 2.2 of SICP by Abelson and Sussman
 | Written by Marcel Goh, last updated 21 May 2018
 |#

; Exercise 2.17
(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

; Exercise 2.18 (iterative)
(define (my-reverse items)
  (define (rev-iter new old)
    (if (null? (cdr old))
        (cons (car old) new)
        (rev-iter (cons (car old) new)
                  (cdr old))))
  (rev-iter '() items))

; Exercise 2.19
(define (cc amount coin-values)
  (define (first-denomination coins) (car coins))
  (define (except-first-denomination coins) (cdr coins))
  (define (no-more? coins) (null? coins))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))

; Exercise 2.20 (iterative)
(define (same-parity n . rest)
  (define (same-iter n new rest)
    (if (null? rest)
        new
        (same-iter n
                   (if (equal? (even? n) (even? (car rest)))
                       (append new (list (car rest)))
                       new)
                   (cdr rest))))
  (same-iter n (list n) rest))

; Exercise 2.21
(define (cond-square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (cond-square-list (cdr items)))))
(define (map-square-list items)
  (map square items))

#| Exercise 2.25:
 | 1. (car (cdr (car (cdr (cdr first)))))
 | 2. (car (car second))
 | 3. (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr third))))))))))))
 |#

; Exercise 2.27
(define (deep-reverse items)
  (cond ((null? items) '())
        ((pair? (car items))
         (append (deep-reverse (cdr items)) (list (deep-reverse (car items)))))
        (else (append (deep-reverse (cdr items)) (list (car items))))))

; Exercise 2.28
(define (fringe items)
  (cond ((null? items) '())
        ((pair? (car items))
         (append (fringe (car items)) (fringe (cdr items))))
        (else
         (append (list (car items)) (fringe (cdr items))))))

; Exercise 2.29 (What a mess! But it works :P)
(define (make-mobile left right)
  (list left right))
; length is scalar value, structure may be a scalar weight or another mobile
(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))
(define (left-weight mobile)
   (if (pair? (branch-structure (left-branch mobile)))
       (weight-mobile (branch-structure (left-branch mobile)))
       (branch-structure (left-branch mobile))))
(define (right-weight mobile)
   (if (pair? (branch-structure (right-branch mobile)))
       (weight-mobile (branch-structure (right-branch mobile)))
       (branch-structure (right-branch mobile))))
(define (weight-mobile mobile)
  (+ (left-weight mobile) (right-weight mobile)))
(define (balanced? mobile)
  (and (= (* (left-weight mobile) (branch-length (left-branch mobile)))
          (* (right-weight mobile) (branch-length (right-branch mobile))))
       (if (pair? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           #t)
       (if (pair? (branch-structure (right-branch mobile)))
           (balanced? (branch-structure (right-branch mobile)))
           #t)))
; d) if the structure because cons instead of list, only the selectors would change

; Exercise 2.30 (without the use of map)
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree))
         (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

; Exercise 2.31
(define (tree-map f tree)
  (cond ((null? tree) '())
        ((not (pair? tree))
         (f tree))
        (else (cons (tree-map f (car tree))
                    (tree-map f (cdr tree))))))

; Exercise 2.32 (This one made me feel smart!)
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
                          rest)))))
#| (We are reducing the list each time, but before we can do that, we must stick the car
 | of the list onto the reduced subsets list. You can see this visually: Moving rightwards,
 | each new number is applied to the full list before it.
 |#

; Exercise 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (my-length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; Exercise 2.35 (makes use of fringe from above)
(define (count-leaves t)
  (accumulate (lambda (x y) (+ y 1))
              0
              (map (lambda (x) x) (fringe t))))
; My solution was to find the length of the flattened tree, so the map function here is redundant.

; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x)
         (accumulate + 0 (map * v x)))
       m))
(define (transpose mat)
  (accumulate-n cons
                '()
                mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (cols)
                  (dot-product row cols))
                cols))
         m)))

; Exercise 2.38: 3/2, 1/6, (1 (2 (3 ()))), (((() 1) 2) 3)
; For fold-left and fold-right to produce the same values, op should be commutative.

; Exercise 2.39
(define (right-reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))
(define (left-reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

; Exercise 2.40
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (enumerate-interval m n)
  (if (> m n)
    '()
    (cons m (enumerate-interval (+ m 1) n))))
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; Exercise 2.41
(define (ordered-triples n s)
  (define all-triples
    (flatmap (lambda (i)
               (flatmap (lambda (j)
                          (map (lambda (k) (list i j k))
                                 (enumerate-interval (+ j 1) n)))
                        (enumerate-interval (+ i 1) n)))
             (enumerate-interval 1 n)))
  (define sum-valid?
    (lambda (triple)
      (= s (accumulate + 0 triple))))
  (filter sum-valid? all-triples))
; My solution set is ordered from lowest to highest.

; Exercise 2.42
(define empty-board '())
(define (adjoin-position row column rest)
  (append rest (list (list row column))))
(define (safe? k positions)
  (define row-to-check
    (caar (filter (lambda (pos) (= (cadr pos) k))
                 positions)))
  (define row-match?
    (lambda (pos) (= (car pos) row-to-check)))
  (define rows-safe?
    (<= (accumulate (lambda (x y) (+ y 1)) 0 (filter row-match? positions))
        1))
  (define diag-match?
    (lambda (pos) (= (abs (- (car pos) row-to-check))
                     (abs (- (cadr pos) k)))))
  (define diag-safe?
    (<= (accumulate (lambda (x y) (+ y 1)) 0 (filter diag-match? positions))
        1))
  (and rows-safe? diag-safe?))
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position
                       new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))
; My solution hits max recursion depth at (queens 11). I chose to represent a row-column pair as a list.
; In retrospect, a cons might have been a more elegant representation.
