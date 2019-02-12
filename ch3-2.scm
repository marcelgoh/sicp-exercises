#| Solutions to selected exercises from Structure and Interpretation of Computer Programs
 | by Abelson and Sussman, Chapter 3.2. Last updated 11 February 2019 by Marcel Goh
 |#

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
