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
## Exercise 3.20

First calling `(define x (cons 1 2))`:

```
           +--------------------------------------------------+
           | cons: --------------------+                      |
global --> | car: ...                  |                      |
env        | cdr: ...                  |                      |
           | set-car!: ...             |                      |
           | set-cdr!: ...             |                      |
           | x: -----------------------+---------+            |
           +---------------------------+---------+------------+
parameters: x, y                       V     A   |   A
body: (define set-x! ...)           --- ---  |   |   |
      (define set-y! ...)    <-----+-0 | 0-+-+   |   |
      (define dispatch ...)         --- ---      |   |
                                                 |   |
             +-----------------------------------+   |
             |                                       |
             V        +-----------------+            |
          --- ---     |   x: 1          | -----------+
         | 0 | 0-+--> |   y: 2          | <- E1 (define x (cons 1 2))
          -+- ---     ||- set-x!:       |
           |          ||  set-y!: ------+--+
           |          ||  dispatch: ... |  |
           V          ++----------------+  |
parameters: m          |    A         A----+-+
body: (cond ...)       +--+ |              V |
                          | |           --- -+-
                          V |          | 0 | 0 |
                       --- -+-          -+- ---
parameters: v     <---+-0 | 0 |          +------> parameters: v
body: (set! x v)       --- ---                    body: (set! y v)
```

Now creating `z` and calling `(set-car! (cdr z) 17)`:

```
global env:
+------------------------+
| <other vars>           |
| x ---------------------+--> <proc with body of dispatch and pointer to E1>
| z ---------------------+--> <proc with body of dispatch and pointer to E2>
+------------------------+
             A         A--------------+
E1:          |              E2:       |
+--------------------+     +--------------------+
| x: 1 (becomes 17)  |     | x: x               |
| y: 2               |     | y: x               |
| <other procedures> |     | <other procedures> |
+--------------------+     +--------------------+
          A                           A
          |                           |
          |                     E3:   |
          |                     +---------+         evaluating (cdr z)
          |                     | m: 'cdr |  -----> returns the variable x
   E4:    |                     +---------+         in the global env
   +--------------+
   | m: 'set-car! | <---- calling set-car! on (cdr z) = x,
   +--------------+
        A
   E5:  |
   +-------+
   | v: 17 | <------ calling set-x! with v = 17
   +-------+
```

This calls `(set x 17)` which changes the value of `x` in the environment `E1`. So calling `(car x)` in the global environment will return `17` (because `x` in the global environment points to `E1`).

## Exercise 3.25

Using `equal?`, simply pass the list as the key to the table we built in Exercise 3.24, because `equal?` can compare list structures.

## Exercise 3.26

Instead of the dummy record pointing to a linked list as a backbone, it should point to the root of a binary search tree like we made in Exercise 2.66.

## Exercise 3.27

The computation is done in an environment where a table with previously computed values is stored. This takes n steps because we note that the computation will be done once for every value from 0 to n. But for all other times the value of `(fib i)` is needed, it is looked up, which if we use an efficient representation of tables like in Exercise 3.26, takes time proportional to `log(n)`, so the actual running time will be something to the order of `n log(n)`. The function will not work if we memoized `fib`, because `fib` calls itself recursively.