#| Solutions to selected exercises from Structure and Interpretation of Computer Programs
 | by Abelson and Sussman. Written by Marcel Goh. Last updated 2 July 2018.
 |#

; Exercise 2.54
(define (my-equal? a b)
  (cond ((not (eq? (list? a) (list? b)))
         #f)
        ((or (and (not (list? a)) (not (list? b)))
             (or (eq? a '()) (eq? b '())))
         (eq? a b))
        ((and (list? a) (list? b))
         (and (my-equal? (car a) (car b))
              (my-equal? (cdr a) (cdr b))))))

; Exercise 2.55
; 'x is simply shorthand for (quote x) so ''x is equivalent to (quote (quote x))
; ''abracadabra is (quote (quote abracadabra)) which is the same as
; (quote abracadabra), the car of which is simply "quote"

; DERIV example
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (variable? e) (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
; Exercise 2.56 (Part 1)
(define (make-exponentiation b e) 
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
; (End 2.56 Part 1)
; Exercise 2.57
(define (addend s) (cadr s))
(define (augend s) 
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))
(define (multiplier p) (cadr p))
(define (multiplicand p) 
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))
; (End Exercise 2.57)
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ; Exercise 2.56 (Part 2)
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp) 
                                                          (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        ; (End 2.56 Part 2)
        (else
          (error "unknown expression type: DERIV" exp))))

; SETS AS LISTS Example
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
       (let ((x1 (car set1)) (x2 (car set2)))
         (cond ((= x1 x2)
                (cons x1 (intersection-set (cdr set1)
                                           (cdr set2))))
               ((< x1 x2)
                (intersection-set (cdr set1) set2))
               ((< x2 x1)
                (intersection-set set1 (cdr set2)))))))
; Exercise 2.61
(define (adjoin-set x set)
  (if (or (null? set) (< x (car set)))
      (cons x set)
      (cons (car set) (adjoin-set x (cdr set)))))
; Exercise 2.59
(define (unordered-union-set set1 set2)
  (if (null? set1) 
      set2
      (unordered-union-set (cdr set1) (adjoin-set (car set1) set2))))
#| Exercise 2.60: element-of-set? would remain the same, adjoin-set would become a simple (cons x set),
 |                intersection-set would remain the same, union-set would become (append set1 set2).
 |                element-of-set? (and consequently intersection-set) might become slightly slower because the 
 |                lists are larger but adjoin-set and union-set become much faster. This representation
 |                might be useful if computation is precious but space is abundant. 
 |#
; Exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-set (cdr set1) set2)))
                      ((> x1 x2)
                       (cons x2 (union-set set1 (cdr set2)))))))))

; SETS AS BINARY TREES Example
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (t-element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (t-element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))
; Exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
#| a. The two procedures produce the same result for each of the three trees in Fig. 2.16.
 | b. tree-list-1 is slower because it calls append which needs to cdr down the lists. 
 |#

; Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
                (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))
#| a. partial-tree finds the approximate centre of the list, sets it as the entry, then recursively
 |    calls partial-tree on the left side and right side on the centre to create the branches.
 |    (5 (1 () (2 () ())) (9 (7 () ()) (11 () ())))
 | b. The order is proportional to the size of the list so it's O(n). 
 |#

; Exercise 2.65
(define (t-union-set set1 set2)
  (list->tree (union-set (tree->list-2 set1) 
                         (tree->list-2 set2))))
(define (t-intersection-set set1 set2)
  (list->tree (intersection-set (tree->list-2 set1) 
                                (tree->list-2 set2))))
