# SICP Chapter 3 Written Solutions

Some written solutions to exercises in Chapter 3 of SICP. Written by Marcel Goh and last updated on 11 February 2019.

## Exercise 3.9

Recursive version:

```
           +--------------------------------------------------+
           | factorial: --                                    |
global --> |             |                                    |
env        | +-----------+                                    |
           +-+------------------------------------------------+
             |     A              A               A
             |     |              |               |
             V     |           +-----+         +-----+
          --- ---  |    E1 --> | n:6 |  E2 --> | n:5 |  ... ETC.
         | 0 | 0-+-+           +-----+         +-----+
          -+- ---
           |
           V
  parameters: n
  body: (if (= n 1) 1 (* n (factorial (- n 1))))
```

Iterative version:

```
           +--------------------------------------------------------+
           | factorial: ...                                         |
global --> | fact-iter: ...  [pointers to procedures as above]      |
env        |                                                        |
           +--------------------------------------------------------+
(factorial 6) A               A                               A
              |               |                               |
           +-----+         +-------------+                 +-------------+
    E1 --> | n:6 |  E2 --> | product:1   |          E3 --> | product:1   |     ... ETC.
           +-----+         | counter:1   |                 | counter:2   |
                           | max-count:6 |                 | max-count:6 |
                           +-------------+                 +-------------+
    (fact-iter 1 1 n)   (if (> counter max-count)          (if (> counter max-count
                            product                             product
                            (fact-iter (* counter product)      (fact-iter (* counter product)
                                       (+ counter 1)                       (+ counter 1)
                                       max-count))                         max-count))
```

## Exercise 3.10

```
           +--------------------------------------------------+
           |    make-withdraw: ...                            |
global --> |    W2: ---+                                      |
env        | +- W1:    |                                      |
           +-+---------+--------------------------------------+
             |         |          A A
           +-+---------+----------+-+--------> parameters: initial-amount
           | |       +-+----------+-+--------> body: (let ((balance initial-amount))
           | V       | V          | |                  (lambda (amount)
          -+- ---   -+- ---       | |                    (if (>= ... )
         | 0 | 0 | | 0 | 0 |      | |                       ... )))
          -+- -+-   --- -+-       | |
             +-+-------+-+--------+-+--------> parameters: balance
             | |       +-+--------+-+--------> body: (lambda (amount) ...)
             | V       | V        | |
            -+- ---   -+- ---     | |
           | 0 | 0 | | 0 | 0 |    | |
            --- -+-   --- -+-     | |
               +-+---------+------+-+--------> parameters: amount
               | |       +-+------+-+--------> body: (if (>= balance amount)
               | V       | V      | |                    (begin (set! ... ) ...)
              -+- ---   -+- ---   | |                    "Insufficient funds")
             | 0 | 0 | | 0 | 0-+--+-+-----------+
              --- -+-   --- ---   | |           |
                   |       +------+ +-----------+-----+
                   V       |                    V     |
                +-------------+             +--------------+
          E1 -> | balance: 50 |       E2 -> | balance: 100 |
                +-------------+             +--------------+
```

## Exercise 3.11

Environment structure of interactions with `acc`:

```
           +--------------------------------------------------+
           | make-account: ------------+                      |
global --> | acc: ---------------------+---------+            |
env        |                           |         |            |
           +---------------------------+---------+------------+
parameters: balance                    V     A   |   A
body: (define withdraw ...)         --- ---  |   |   |
      (define deposit ...)   <-----+-0 | 0-+-+   |   |
      (define dispatch ...)         --- ---      |   |
                                                 |   |
             +-----------------------------------+   |
             |                                       |
             V        +-----------------+            |
          --- ---     |   balance: 50   | <----------+
         | 0 | 0-+--> ||- deposit: ...  | <- E1 (define acc (...))
          -+- ---     ||  withdraw: ----+--+
           |          ||  dispatch: ... |  |
           V          ++----------------+  |
parameters: m          |    A      A  A----+-+
body: (cond ...)       +--+ |      |       V |
                          | |      |    --- -+-
parameters: amount        V |      |   | 0 | 0 |
body: (set!            --- -+-     |    -+- ---
        balance   <---+-0 | 0 |    |     +------> parameters: amount
        (+ balance     --- ---     |              body: ...
           amount))                |
       balance           +-------------+
                         | m: 'deposit | <-- E2 (acc 'deposit)
                         +-------------+
+-----------------+
|   balance: 90   |  <-- <E1 after deposit>
|   deposit: ...  |                        +--------------+
|   withdraw: ... |  <-------------------- | m: 'withdraw | <-- E3 (acc 'withdraw)
|   dispatch: ... |  <------+              +--------------+
+-----------------+         |     +------------+
                            +---- | amount: 60 | <- E4 ((acc 'withdraw) 60)
                                  +------------+
```

Now running `(define acc2 (make-account 100))`:

```
           +--------------------------------------------------+
           | make-account: ...                                |
global --> | acc: -------+                                    |
env        | acc2 -------+-----------------------+            |
           +-------------+-----------------------+------------+
parameters: m  <---------+---------------------+ |
body: ...                V                     | V
         A            --- ---                 -+- ---
         +-----------+-0 | 0 |               | 0 | 0 |
                      --- -+-                 --- -+-
                           |                       |
                           V                       V
                +---------------+           +---------------+
                | balance: 30   |           | balance: 100  |
                | deposit: ...  |           | deposit: ...  |
                | withdraw: ... |           | withdraw: ... |
                | dispatch: ... |           | dispatch: ... |
                +---------------+           +---------------+
              <E1 after withdrawal>                 E2
```

## Exercise 3.12

The responses are `(b)` and `(b c d)` respectively. The `append` function does not mutate `x`, so the `cdr` of `x` remains `(b)` as it was when `x` was originally defined. However, calling `append!` does mutate the `cdr` of `x` by attaching the whole of `y` onto it, so `(cdr x)` now returns `(b c d)`.

## Exercise 3.13

```
          +----------------------------------+
          V                                  |
         --- ---      --- ---      --- ---   |
z: ---> | 0 | 0-+--> | 0 | 0-+--> | 0 | 0 -+-+
         -+- ---      -+- ---      -+- ---
          |            |            |
          V            V            V
         'a           'b           'c
```

Trying to compute `(last-pair z)` results in an infinite loop as the pointers are followed round-and-round.

## Exercise 3.14

The function `mystery` reverses a list destructively. To save space, I won't draw the diagrams, but `v` before the call is `(a b c d)`. After the call, `v` is simply `(a)` and `w` is `(d c b a)`.

## Exercise 3.15

After calling `(set-to-wow! z1)` and `(set-to-wow! z2)` we have the following pointer diagrams:

```
x ----->   --- ---       --- ---          --- ---       --- ---
  +---->  | 0 | 0-+---> | 0 | / |    +-> | 0 | 0-+---> | 0 | / |
  |  +->   -+- ---       -+- ---     |    -+- ---       --- ---
  |  |      |             |          |     V             V
  |  |      V             V          |    'wow            'b
  |  |    'wow            'b         |
  |  |                           +---+           --- ---       --- ---
  |  +------------------+        |          +-> | 0 | 0-+---> | 0 | / |
  +-------+             |        |          |    -+- ---       -+- ---
         -+- ---       -+- ---   |          |     V             V
z1 ---> | 0 | 0-|---> | 0 | / |  |          |    'a            'b
         --- ---       --- ---   |          ---+
                                -+- ---       -+- ---
                       z2 ---> | 0 | 0-+---> | 0 | / |
                                --- ---       --- ---
```

## Exercise 3.16

Here are the box-and pointer diagrams:

```
            --- ---      --- ---      --- ---
three: --> | 0 | 0-+--> | 0 | 0-+--> | 0 | / |
            --- ---      --- ---      --- ---
             V            V            V
             1            2
                                      --- ---
           --- ---        seven: --> | 0 | 0 |
four: --> | 0 | 0 |                   -+- -+-
           -+- -+-                     ++ ++
            |   |                       V V
            |   +--------+              --- ---
            V            V             | 0 | 0 |
           --- ---      --- ---         -+- -+-
          | 0 | 0-+--> | 0 | / |         ++ ++
           -+- ---      -+- ---           V V
            V            V                --- ---
            1            2          1 <--+-0 | / |
             +-------------------------+  --- ---
             V                         |
            --- ---      --- ---      -+- ---
never: --> | 0 | 0-+--> | 0 | 0-+--> | 0 | / |
            -+- ---      -+- ---      --- ---
             V            V
            'a           'b
```

And here are the definitions of these structures in code:

```scheme
; auxiliary structures
(define x (list 1 2))
(define y (list 'a 'b 'c))
(define p (list 1))
(define q (cons p p))

(define three (list 1 2 3))      ; (count-pairs three) returns 3
(define four (cons x (cdr x)))   ; (count-pairs four) returns 4
(define seven (cons q q))        ; (count-pairs seven) returns 7
(define never                    ; (count-pairs never) never returns
  (begin
    (set-car! (last-pair y) y)
    y))
```
