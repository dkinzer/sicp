(declare (usual-integrations))
(load '("lib/assert" "lib/math"))

; Exercise 1.9:
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

(display (string "TEST time for : (+1.9a 1e4 1e4)"))
(define +1.9a-times
  (time (lambda () (+1.9a 1e4 1e4))))

(display (string "TEST time for : (+1.9b 1e4 1e4)"))
(define +1.9b-times
  (time (lambda () (+1.9b 1e4 1e4))))

(assert (= +1.9a-times +1.9b-times)
        "There are no time differences between procedure +1.9a and +1.9b.")

; Exercise 1.10:
; The following procedure computes a mathematical function called Ackermann's function.
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
  (display (string "n: " n ", result: " (A 0 n)))
  (newline)
  (A 0 n))

(display (string "TEST: (A 0 n)"))
(newline)
(f 0)
(f 1)
(f 2)
(f 3)

(assert (= (* 2 50) (f 50))
        "(f n) computes to 2 * n")

(define (g n)
  (display (string "n: " n ", result: " (A 1 n)))
  (newline)
  (A 1 n))

(display (string "TEST: (A 1 n)"))
(newline)
(g 0)
(g 1)
(g 2)
(g 3)

(assert (= 1024 (g 10))
        "(g n) computes to 2^n")

(define (h n)
  (display (string "n: " n ", result: " (A 2 n)))
  (newline)
  (A 2 n))

(display (string "TEST: (A 2 n)"))
(newline)
(h 0)
(h 1)
(h 2)
(h 3)
(assert (= 65536 (h 4))
        "(h n) computes to 2^2^2^(n - 2)")
