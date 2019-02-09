#| Solutions to selected exercises from Structure and Interpretation of Computer Programs
 | by Abelson and Sussman, Chapter 2.5. Last updated 8 February 2019 by Marcel Goh
 |#

; install all packages
(define (install-all)
  (install-complex-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-coercion-package)
  (install-polynomial-package)
  'done)

; helper put/get operations for testing
(define *op-table* (make-equal-hash-table))
(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))
(define (get op type)
  (hash-table/get *op-table* (list op type) #f))

; helper put/get operations for coercion table
(define *coercion-table* (make-equal-hash-table))
(define (put-coercion type-from type-to proc)
  (hash-table-set! *coercion-table* (list type-from type-to) proc))
(define (get-coercion type-from type-to)
  (hash-table-ref/default *coercion-table* (list type-from type-to) #f))

; coercion procedures
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(define (install-coercion-package)
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  'done)

; tagged data
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum)
         (car datum))
        ((number? datum)                                   ; Exercise 2.78
         'scheme-number)
        (else
         (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum)
         (cdr datum))
        ((number? datum)                                   ; Exercise 2.78
         datum)
        (else
         (error "Bad tagged datum: CONTENTS" datum))))
(define (apply-generic op . args)
  (define (transform a1 a2)                                ; Exercise 2.84
    (let ((type1 (type-tag a1))
          (type2 (type-tag a2)))
      (cond ((equal? type1 type2) a1)
            ((get 'raise (list type1))
             (transform ((get 'raise (list type1)) (contents a1)) a2))
            (else #f))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (cond ((transform a1 a2)
                       (apply-generic op (transform a1 a2) a2))
                      ((transform a2 a1)
                       (apply-generic op a1 (transform a2 a1)))
                      (else (error "No method for these types: APPLY-GENERIC"
                                   (list op type-tags)))))
              (error "No method for these types: APPLY-GENERIC"
                     (list op type-tags)))))))

; generic selectors
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y))
(define (raise x) (apply-generic 'raise x))
(define (drop x)                                           ; Exercise 2.85
  (let ((proj (get 'project (list (type-tag x)))))
    (if proj
        (let ((dropped (proj (contents x))))
          (if (equ? x (raise dropped))
              (drop dropped)
              x))
        x)))
(define (neg x) (apply-generic 'neg x))                    ; Exercise 2.88
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (equ? x y) (apply-generic 'equ? x y))              ; Exercise 2.79
(define (=zero? x) (apply-generic '=zero? x))              ; Exercise 2.80

; rectangular representation 
(define (install-rectangular-package)
  ; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)

  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; polar representation
(define (install-polar-package)
  ; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; complex number package
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (install-complex-package)
  ; imported procedures from rectangular and polar packages
  (install-rectangular-package)
  (install-polar-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ; given code for Exercise 2.77
  (put 'real-part '(complex) (lambda (z) ((get 'real-part (list (type-tag z))) (contents z))))
  (put 'imag-part '(complex) (lambda (z) (apply-generic 'imag-part z)))
  (put 'magnitude '(complex) (lambda (z) (apply-generic 'magnitude z)))
  (put 'angle '(complex) (lambda (z) (apply-generic 'angle z)))
  ; Exercise 2.77
  ; This works because now there is a magnitude procedure defined on complex objects.
  ; This applies the magnitude procedure on the rectangular object which in turn
  ; applies the magnitude procedure on the real-imag pair (3 . 4), returning 5
  (define (complex-equal? z1 z2)                           ; Exercise 2.79
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (complex-equal? z1 z2)))
  (put '=zero? '(complex)                                  ; Exercise 2.80
       (lambda (z) (and (= (real-part z) 0)
                        (= (imag-part z) 0))))
  (put 'project                                            ; Exercise 2.85
       '(complex)
       (lambda (z) (make-scheme-number (real-part z))))
  'done)

; ordinary (real) number package
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (define (scheme-number->complex n)                       ; Exercise 2.83
    (make-complex-from-real-imag (contents n) 0))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'neg '(scheme-number)                               ; Exercise 2.88
       (lambda (x) (tag (- x))))
  (put 'raise '(scheme-number)
       (lambda (x) (scheme-number->complex x)))
  (put 'project '(scheme-number)                           ; Exercise 2.85
       (lambda (x)
         (let ((rational
                 (rationalize (inexact->exact (contents x)) 1/100)))
           (make-rational (numerator rational) (denominator rational)))))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)                ; Exercise 2.79
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)                            ; Exercise 2.80
       (lambda (x) (= x 0)))
  'done)

; rational number package
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (install-rational-package)
  ; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (rational->scheme-number rat)                    ; Exercise 2.83
    (make-scheme-number (exact->inexact (/ (numer rat) (denom rat)))))
  ; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'raise '(rational)
       (lambda (r) (rational->scheme-number r)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (define (rational-equal? x y)                            ; Exercise 2.79
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (rational-equal? x y)))
  (put '=zero? '(rational)                                 ; Exercise 2.80
       (lambda (x) (= (numer x) 0)))
  'done)

; provided code for 2.5.3
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (install-polynomial-package)
  ; polynomial representation
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable v2) (eq? v1 v2)))
  ; terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-term term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ; adding and multiplying
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order L1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
            (make-term (+ (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
            (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polynomials not in same variable: ADD-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polynomials not in same variable: MUL-POLY"
               (list p1 p2))))
  (define (zero?-poly p)                                   ; Exercise 2.87
    (let ((tlist (term-list p)))
      (if (empty-termlist? terms)
          #t
          (and (=zero? (coeff (first-term terms)))
               (zero-poly? (make-poly (variable p) (rest-terms terms)))))))
  (define (negate-terms tlist)                             ; Exercise 2.88
    (if (empty-termlist? tlist)
        (the-empty-termlist)
        (let ((first (first-term terms)))
          (adjoin-term
            (make-term (order first) (neg (coeff first)))
            (negate-terms (rest-terms terms))))))
  (define (negate-poly p)
    (make-poly (variable p) (negate-terms (term-list p))))
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))
  (define (div-terms dend dsor)                            ; Exercise 2.91
    (if (empty-termlist? dend)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term dend))
              (t2 (first-term dsor)))
          (if (> (order dsor) (order dend))
              (list (the-empty-termlist) dend)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((new-term (make-term new-o new-c)))
                  (let ((product (mul-terms dsor (list new-t))))
                    (let ((difference (add-terms dend (negate-terms product))))
                      (let ((rest-of-result (div-terms difference dsor)))
                        (list (cons new-term (car rest-of-result))
                              (cadr rest-of-result)))))))))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((answer (div-terms (term-list p1)
                                 (term-list p2))))
          (list (make-poly (variable p1) (car answer))
                (make-poly (variable p2) (cadr answer))))
        (error "Polynomials not over same variable: DIV-POLY"
               (list p2 p2))))

  ; interface to rest of system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)                               ; Exercise 2.87
       (lambda (p) (zero-poly? p)))
  (put '=neg '(polynomial)                                 ; Exercise 2.88
       (lambda (p) (tag (negate-poly p))))
  (put 'sub '(polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'div '(polynomial)                                  ; Exercise 2.91
       (lambda (p1 p2) (list (tag (car (div-poly p1 p2)))
                             (tag (cadr (div-poly p1 p2))))))
  'done)
