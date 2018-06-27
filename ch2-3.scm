#| Solutions to selected exercises from Structure and Interpretation of Computer Programs
 | by Abelson and Sussman. Written by Marcel Goh. Last updated 24 June 2018.
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
(define (augend s) (cddr s))
(define (multiplier p) (cadr p))
(define (multiplicand p) (cddr p))
(define (make-sum . a)
  (cond ((list? (augend a))
         (make-sum (addend a) (make-sum (augend a))))
        ((=number? (addend a) 0) (augend a))
        ((=number? (augend a) 0) (addend a))
        ((and (number? (addend a)) (number? (augend a)))
         (+ (addend a) (augend a)))
        (else (list '+ (addend a) (augend a)))))
(define (make-product . m)
  (cond ((list? (multiplicand m))
         (make-product (multiplier m) (make-sum (multiplicand m))))
        ((or (=number? m1 0) (=number? m2 0)) 0)
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

