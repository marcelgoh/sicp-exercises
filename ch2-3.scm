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


