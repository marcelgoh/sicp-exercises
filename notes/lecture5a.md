# SICP Lecture Notes

## Lecture 5A

### Assignment and mutation

So far, we've restricted ourselves to functional programs.

+ Functional programs encode mathematical truths
+ Processes evolved by such programs can be understood by substitution
+ Methods may be distinguished by the choice of truths expressed

We're going to add an awful thing by allowing mutation of variables:

```
<before>
(set! <variable> <value>)
<after>
```
This introduces into our programs a notion of time. We can now iterate.
```
(define count 1)
(define (demo x)
  (set! count (+ 1 count))
  (+ x count))
>> (demo 3)
5
>> (demo 3)
6
```
Calling `demo` twice returns different values each time! Strictly speaking, `demo` is not a function, so the substitution model is now dead. We've lost our model of computation. Informally, names no longer refer to values; rather, they now refer to a place that we can store a value.  
Let's look at a function, `fact`, that we've seen before:
```
(define (fact n)
  (define (iter m i)
    (cond ((> i n) m)
          (else (iter (* i m) (+ i 1)))))
  (iter 1 1))
```
Here is the iterative version, which will be familiar to FORTRAN programmers:
```
(define (fact n)
  (let ((i 1) (m 1))
    (define (loop)
      (cond ((> i n) m)
            (else
              (set! m (* i m))
              (set! i (+ i 1))
              (loop))))))
```
The model of computation that corresponds to procedural programming is called the __environment__ model. We say that a variable _v_ is __bound__ in an expression _e_, if the meaning of _e_ is unchanged by the uniform replacement of a variable _w_, not occuring in _e_, for every occurence of _v_ in _e_.

### Bound variables

In computer science, the symbol that binds variables is λ:
```
(λ (y) ((λ (x) (* x y)) 3))
```
If you take another variable that doesn't appear in this expression, say `w`, and substituted it into every occurrence of `y`, the expression would be exactly the same. However, sometimes variables are not bound:
```
(λ (x) (* x y))
```
Here `y` is not bound. Instead, it is called a __free__ variable. Consider the following expression:
```
(λ (x) (λ (y) (* x y)))
```
Here `*` is a free variable, because if it were replaced with another variable, say `+`, then the meaning of the expression would change.

### Environments

We know now that variables are names of places that values can be stored. An environment is a list of names and the values that are currently associated with them. This can be a function or a table, and it is a structure that's made up of frames.  
When we create a procedure object, we need to have a pointer to environment as well as a pointer to the procedure code. Now we have new rules of evaluation:

1. A procedure object is applied to a set of arguments by constructing a frame, binding the formal parameters of the procedure to the actual arguments of the call, and then evaulating the body of the procedure in the context of the new environment constructed. The new frame has as its enclosing environment the environment part of the procedure object being applied.

2. A λ-expression is evaluated relative to a given environment as follows: a new procedure object is formed, combining the text (code) of the λ-expression with a pointer to the environment of evaluation.

An environment can be thought of as a sequence of frames all pointing to each other. (Or just the pointer to the first one, because that captures all of them.)

### Benefits of mutation

Why did we choose to destroy much of the mathematical foundations our programs used to have? Well, we can do interesting things with mutation:
```
(define make-counter
  (λ (n)
    (λ ()
      (set! n (+ 1 n))
      n)))
(define c1 (make-counter 0))
(define c2 (make-counter 10))
>> (c1)
1
>> (c2)
11
>> (c1)
2
>> (c2)
12
```
We have two counters, each with their individual local state. This brings up the question of what an object is. Objects divide the world into things that mostly have nothing to do with one another, allowing some economy of thought. How do we tell if two objects are the same? One way to do it is to mutate one of them in some way and observe if the other one changes as well. But how do we tell if something has changed? We need to observe it before and after the change. Introducing assignments and objects makes thinking about our programs a lot messier, because we have to worry about the dimension of time.  

We say that an action _A_ had an effect on an object _X_ (or equivalently, that _X_ was changed by _A_) if some property _P_ which was true of _X_ before _A_ became false of _X_ after A. We say that two objects _X_ and _Y_ are the same if any action which has an effect on _X_ has the same effect on _Y_.  

Sometimes, we have to use mutation, but generally, it should be avoided.

### An application of mutation: Monte Carlo experiments
This is a general procedure for running Monte Carlo experiments:
```
(define (monte-carlo trials experiment)
  (define (iter remaining passed)
    (cond ((= remaining 0)
           (/ passed trials))
          (experiment)
           (iter (-1+ remaining)
                 (1+ passed)))
          (else
            (iter (-1+ remaining)
                  (passed))))
  (iter trials 0))
```
It relies on a procedure for generating random numbers, which might look something like this,
```
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))
```
where `rand-update` is a mathematical function for finding random numbers from a seed, `random-init`. The procedure hides a piece of local state, i.e. the value currently in the random number generator. If you were going to write a functional version of the Monte Carlo procedure, you would have to pass it the random seed and the random number generator each time. So in this case, mutation surely gives us a more elegant solution.