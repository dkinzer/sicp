(declare (usual-integrations))
(load '("lib/assert" "lib/math"))

; {{{1 1.2.1  Linear Recursion and Iteration (1.9 - 1.10)

; {{{2 Exercise 1.9:
; Each of the following two procedures defines a method for adding two positive
; integers in terms of the procedures inc, which increments its argument by 1,
; and dec, which decrements its argument by 1.
(define (+1.9a a b)
  (if (= a 0)
    b
    (inc (+1.9a (dec a) b))))

; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
;

(define (+1.9b a b)
  (if (= a 0)
    b
    (+1.9b (dec a)  (inc b))))

; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9

; Using the substitution model, illustrate the process generated by each
; procedure in evaluating (+ 4 5). Are these processes iterative or recursive?
;
; The firt process is recursive while the second is iterative.
(set-gc-notification! #t)

(term-display (string "TEST time for : (+1.9a 1e4 1e4)"))
(define +1.9a-times
  (time (lambda () (+1.9a 1e4 1e4))))

(term-display (string "TEST time for : (+1.9b 1e4 1e4)"))
(define +1.9b-times
  (time (lambda () (+1.9b 1e4 1e4))))

(assert (= +1.9a-times +1.9b-times)
        "There are no time differences between procedure +1.9a and +1.9b.")

; {{{2 Exercise 1.10:
; The following procedure computes a mathematical function called Ackermann's
; function.
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0)  (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
; What are the values of the following expressions?
;
; (A 1 10)
; (A (- 1 1) (A 1 (- 10 1)))
; (A 0 (A 1 9))
; (* 2 (A 1 9))
; (* 2 (A (- 1 1) (A 1 (- 9 1))))
; (* 2 (A 0 (A 1 8)))
; (* 2 (* 2 (A 1 8)))
; (* 2 (* 2 (* 2 (A 1 7))))
; (* 2 (* 2 (* 2 (A 1 7))))
; ...
; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 ( * 2 (* 2 (* 2 (A 1 1))))))))))
; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 ( * 2 (* 2 (* 2 (2))))))))))
; ...
; 2^10 = 1024
(assert (= 1024 (A 1 10))
        "(A 1 10) evaluates to 1024")
;
;
(assert (= 65536 (A 2 4) (A 3 3) )
        "(A 2 4) and (A 3 3) are equal to 65536")

; Consider the following procedures, where A is the procedure defined above:
; Give concise mathematical definitions for the functions computed by the
; procedures f, g, and h for positive integer values of n.
(define (f n)
  (term-display (string "n: " n ", result: " (A 0 n)))
  (newline)
  (A 0 n))

(term-display (string "TEST: (A 0 n)"))
(newline)
(f 0)
(f 1)
(f 2)
(f 3)

(assert (= (* 2 50) (f 50))
        "(f n) computes to 2 * n")

(define (g n)
  (term-display (string "n: " n ", result: " (A 1 n)))
  (newline)
  (A 1 n))

(term-display (string "TEST: (A 1 n)"))
(newline)
(g 0)
(g 1)
(g 2)
(g 3)

(assert (= 1024 (g 10))
        "(g n) computes to 2^n")

(define (h n)
  (term-display (string "n: " n ", result: " (A 2 n)))
  (newline)
  (A 2 n))

(term-display (string "TEST: (A 2 n)"))
(newline)
(h 0)
(h 1)
(h 2)
(h 3)
(assert (= 65536 (h 4))
        "(h n) computes to 2^2^2^(n - 2)")

; {{{1 1.2.1  Linear Recursion and Iteration (1.11 - 1.13)

; {{{2 Exercise 1.11:
; A function f is defined by the rules that
;
;  1. f(n) = n if (n < 3)
;  2. f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if (n >= 3)
;
; Write a procedure that computes f by means of a recursive process.
(define (f n)
  (if (> 3 n)
    n
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(assert (= -1 (f -1))
        "The procedure #f works as expected for a value less than 3.")
(assert (= (+ 2 (* 2 1) (* 3 0)) (f 3))
        "The procedure #f works as expected for values not less than 3.")

; Write a procedure that computes f by means of an iterative process.
(define (fi n)
  (define (iter i n1 n2 n3)
    (let ((n0 (+ n1 (* 2 n2) (* 3 n3))))
      (if (= i n)
        n0
        (iter (inc i) n0 n1 n2))))
  (if (> 3 n)
    n
    (iter 3 2 1 0 )))

(assert (= -1 (fi -1))
        "The procedure #fi works as expected for a value less than 3.")
(assert (= (+ 2 (* 2 1) (* 3 0)) (fi 3))
        "The procedure #fi works as expected for values not less than 3.")
(assert (= (f 5) (fi 5))
        "The recursive and iterative procedures #f and #fi are equivalent.")

; {{{2 Exercise 1.12:
; The following pattern of numbers is called Pascal's triangle.
;
; n
; -
; 1             1
; 2           1   1
; 3         1   2   1
; 4       1   3   3   1
; 5     1   4   6   4   1
;
; Where n denotes the levels of the Triangle.
;
; The numbers at the edge of the triangle are all 1, and each number inside the
; triangle is the sum of the two numbers above it.  Write a procedure that
; computes elements of Pascal's triangle by means of a recursive process.
;
; {{{3 Answer
; We can see that if k denotes  the kth term of the triangle at
; level n, and there are n terms at level n, then by definition:
;
; P(1, 1) = 1
; P(n, 1) = 1
; P(n, n) = 1
; P(n, k) = P(n-1, k-1) + P(n-1, k)
(define (P n k)
  (cond ((< n 1) (error "The parameter n is out of bounds."))
        ((< k 1) (error "The parameter k is out of bounds."))
        ; The outer terms are always one.
        ((or (= n 1) (= k 1) (= n k))  1)
        (else (+ (P (- n 1) (- k 1))
                 (P (- n 1) k)))))

(assert-error "out of bounds" (lambda () (P 0 1))
        "The procedure #P throws errors for n less than 1.")
(assert-error "out of bounds" (lambda () (P 1 0))
        "The procedure #P throws errors for k less than 1.")
(assert (= 1 (P 1 1))
        "The procedure #P works as expected for the first term of the first level.")
(assert (= 1 (P 5 1))
        "The procedure #P works as expected for the first term of any level.")
(assert (= 1 (P 5 5))
        "The procedure #P works as expected for the last term of any level.")
(assert (= 4 (P 5 2))
        "The procedure #P works as expected for inner terms.")

; {{{2 Exercise 1.13:
; Prove that Fib(n) is the closest integer to ϕ^n/√5, where ϕ = (1 + √5)/2. Hint:
; Let  ѱ = (1 - √5)/2. Use induction and the definition of the Fibonacci numbers
; (see section 1.2.2) to prove that Fib(n) = (ϕ^n - ѱ^n)/√5.
;
; By definition ф^2 = ф + 1
;
; which is satisfied by the values ф, ψ = (1 + √5)/2, (1 - √5)/2
;
; ф^2 = ((1 + √5)/2)^2 = (1 + 2√5 + 5)/4
;     = (6 + 2√5)/4 = (3 + √5)/2
;     = (2 + 1 + √5)/2
;     = (1 + ф)
;
; ψ^2 = (1 - √5)/4 = (1 - 2√5 + 5)/4
;     = (6 - 2√5)/4 = (3 - √5)/2
;     = (2 + 1 - √5)/2
;     = (1 + ψ)
;
; Then,
; ф^3 = ф (ф + 1) = ф^2 + ф = (ф + 1) + ф = 2ф + 1
; ф^4 = ф (2ф + 1) = 2ф^2 + ф = 2(ф + 1) + ф = 2ф + 2 + ф = 3ф + 2
; ф^5 = ф (3ф + 2) = 3ф^2 + 2ф = 3(ф + 1) + 2ф = 3ф + 3 + 2ф = 5ф + 3
; ф^6 = ф (5ф + 3) = 5ф^2 + 3ф = 5(ф + 1) + 3ф = 5ф + 5 + 3ф = 8ф + 5
; ф^7 = ф (8ф + 5) = 8ф^2 + 5ф = 8(ф + 1) + 5ф = 8ф + 8 + 5ф = 13ф + 8
;
; Fib(0) = 0
; Fib(1) = 1
; Fib(2) = 1
; Fib(3) = 2
; Fib(4) = 3
; Fib(5) = 5
; Fib(6) = 8
; Fib(7) = 13
; Fib(8) = 21
; ..
; Fib(n) = Fib(n - 1) + Fib(n - 2) by definition.
;
; Thus we can infer from the above experiment that
;
; ф^n = Fib(n)ф + Fib(n - 1), and
; ψ^n = Fib(n)ψ + Fib(n - 1)
;
; Which we can prove by induction.
;
; First we show that the base case works.
; ф^1 = Fib(1)ф + Fib(0) or
;   ф = ф + 0
;
; Next we solve the n + 1 case:
;
; ф^(n + 1) = ф * ф^2 = ф * (Fib(n)ф + Fib(n - 1))
;   = Fib(n)ф^2 + Fib(n - 1)ф
;   = Fib(n)(ф + 1) + Fib(n - 1)ф
;   = Fib(n)ф + Fib(n) + Fib(n - 1)ф
;   = (Fib(n) + Fib(n - 1))ф + Fib(n)
;   = Fib(n + 1)ф + Fib(n)
;   QED
;
; Next let's prove Fib(n) = (ϕ^n - ѱ^n)/√5 by induction:
;
; The base case is:
; Fib(0) = (ϕ^0 - ѱ^0)/√5
;      0 = (1 - 1)/√5
;      0 = (1 - 1)
;      0 = 0
;
; And, the n + 1 case:
; Fib(n + 1) = (ϕ^n+1 - ѱ^n+1)/√5 by induction:
;            = (Fib(n + 1)ф + Fib(n) - Fib(n + 1)ψ - Fib(n))/√5
;            = (Fib(n + 1)ф - Fib(n + 1)ψ)/√5
;            = (Fib(n + 1)((1 + √5)/2) - Fib(n + 1)((1 - √5)/2))/√5
;            = (Fib(n + 1)/2 + √5Fib(n + 1)/2 - Fib(n + 1)/2  + √5Fib(n + 1)/2)/√5
;            = (√5Fib(n + 1)/2 + √5Fib(n + 1)/2)/√5
;            = (√5Fib(n + 1))/√5
;            = Fib(n + 1)
;            QED
;
;
; If Fib(n) is  the closest integer to ф^n/√5 then in order for
;
; Fib(n) = (ϕ^n - ѱ^n)/√5 to be true (which is proved)
;
; the limit of Fib(n) as n goes to infinity would have to be equal to ф^n/√5.
;
; The limit of ψ^n as n goes to infinity is 0, therefore Fib(n) is the closest integer to ф^n/√5.
;
; QED

; {{{1 1.2.3  Orders of Growth (1.14 - 1.15)

; {{{2 Exercise 1.14:
; {{{3 Problem
; Draw the tree illustrating the process generated by the count-change
; procedure of section 1.2.2 in making change for 11 cents. What are the orders
; of growth of the space and number of steps used by this process as the amount
; to be changed increases?

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)  (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; {{{3 Solution
(assert (= 4 (count-change 11))
        "The tree expasion for exercise 1.14 is correct. This tree grows
        linearly with respect to space but exponentially with respect to
        time:

 let x   = (count-change x)
 let x,y = (cc x y)
 then,

          -39,5 -> 0
         |
 11 - 11,5
         |
         |
         |  -14,4 -> 0
         |  |
         11,4
            |
            |    -9,3 -> 0
            |    |
            |  1,3
            |  | |
            |  | | -5,2 ->0
            |  | | |
            |  | 1,2
            |  |   | 0,1 -> 1
            |  |   | |
            |  |   1,1
            |  |     |
            |  |     1,0 -> 0
            11,3
               |      -4,2 -> 0
               |      |
               |    1,2
               |    | |
               |    | | 0,1 -> 1
               |    | | |
               |    | 1,1
               |    |   |
               |    |   1,0 -> 0
               |  6,2
               |  | |           0,1 -> 1
               |  | |           |
               |  | |           |
               |  | |         1,1
               |  | |         | |
               |  | |         | 1,0 -> 0
               |  | |         |
               |  | |       2,1
               |  | |       | |
               |  | |       | 2,0 -> 0
               |  | |       |
               |  | |     3,1
               |  | |     | |
               |  | |     | 3,0 -> 0
               |  | |   4,1
               |  | |   | |
               |  | |   | 4,0 -> 0
               |  | |   |
               |  | | 5,1
               |  | | | |
               |  | | | 5,0 -> 0
               |  | | |
               |  | 6,1
               |  |   |
               |  |   6,0 -> 0
               |  |                       0,1 -> 1
               |  |                       |
               |  |                       |
               |  |                     1,1
               |  |                     | |
               |  |                     | 1,0 -> 0
               |  |                     |
               |  |                   2,1
               |  |                   | |
               |  |                   | 2,0 -> 0
               |  |                   |
               |  |                 3,1
               |  |                 | |
               |  |                 | 3,0 -> 0
               |  |               4,1
               |  |               | |
               |  |               | 4,0 -> 0
               |  |               |
               |  |             5,1
               |  |             | |
               |  |             | 5,0 -> 0
               |  |             |
               |  |           6,1
               11,2           | |
                  |           | 6,0 -> 0
                  |         7,1
                  |         | |
                  |         | 7,0 -> 0
                  |       8,1
                  |       | |
                  |       | 8,0 -> 0
                  |     9,1
                  |     | |
                  |     | 9,0 -> 0
                  |  10,1
                  |  |  |
                  |  |  10,0 -> 0
                  11,1
                     |
                     11,0 -> 0
        ")
