(declare (usual-integrations))
(load '("lib/assert" "lib/math"))

; {{{1 2.1.1 Example: Arithmetic Operations for Rational Numbers (2.1)
; {{{2 Exercise 2.1:
; {{{3 Problem
;      Define a better version of make-rat that handles both positive and
;      negative arguments.  make-rat should normalize the sign so that if the
;      rational number is positive, both the numerator and denominator are
;      positive, and if the rational number is negative, only the numerator is
;      negative.
;
; {{{3 Solution
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make-rat n d)
  (let ((g (abs (gcd n d)))
        (nn (if (or
                  (and (< n 0) (< d 0))
                  (and (> n 0) (< d 0)))
              (* n -1)
              n))
        (nd (if (or
                  (and (< n 0) (< d 0))
                  (and (> n 0) (< d 0)))
              (* d -1)
              d)))
    (cons (/ nn g) (/ nd g))))

(assert (let ((rat (make-rat 2 -4)))
          (and (= -1 (numer rat))
               (= 2 (denom rat))))
        "The procedure #make-rat works as expected when passed (2 -4)")

(assert (let ((rat (make-rat -2 4)))
          (and (= -1 (numer rat))
               (= 2 (denom rat))))
        "The procedure #make-rat works as expected when passed (-2 4)")

(assert (let ((rat (make-rat -2 -4)))
          (and (= 1 (numer rat))
               (= 2 (denom rat))))
        "The procedure #make-rat works as expected when passed (-2 -4)")

(assert (let ((rat (make-rat 2 4)))
          (and (= 1 (numer rat))
               (= 2 (denom rat))))
        "The procedure #make-rat works as expected when passed (2 4)")

(assert (let ((rat (make-rat 4 -2)))
          (and (= -2 (numer rat))
               (= 1 (denom rat))))
        "The procedure #make-rat works as expected when passed (4 -2)")

; {{{1 2.1.2 Abstraction Barriers (2.2 - 2.3)
; {{{2 Exercise 2.2:
; {{{3 Problem
;      Consider the problem of representing line segments
;      in a plane.  Each segment is represented as a pair of points: a
;      starting point and an ending point.  Define a constructor
;      #make-segment and selectors #start-segment and #end-segment
;      that define the representation of segments in terms of points.
;      Furthermore, a point can be represented as a pair of numbers: the
;      x coordinate and the y coordinate.  Accordingly, specify a
;      constructor #make-point and selectors #x-point and #y-point
;      that define this representation.  Finally, using your selectors
;      and constructors, define a procedure #midpoint-segment that takes
;      a line segment as argument and returns its midpoint (the point
;      whose coordinates are the average of the coordinates of the
;      endpoints).  To try your procedures, you'll need a way to print
;      points:

; {{{3 Solution
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

(assert (equal? (cons 1 2) (make-point 1 2))
        "Procedure #make-point works as expected when passed 1, 2.")
(assert (= 1  (x-point (make-point 1 2)))
        "Procedure #x-point works as expected.")
(assert (= 2  (y-point (make-point 1 2)))
        "Procedure #y-point works as expected.")
(assert (equal? (cons 0 0)
                (start-segment (make-segment (make-point 0 0) (make-point 2 2))))
        "Procedure #start-segment and #make-segment work as expected.")
(assert (equal? (cons 2 2)
                (end-segment (make-segment (make-point 0 0) (make-point 2 2))))
        "Procedure #end-segment works as expected.")
(assert (equal? (cons 1 1) (midpoint-segment (make-segment (make-point 0 0) (make-point 2 2))))
        "Procedure #midpoint-segment works as expected.")
(assert (equal? (cons 3/2 3/2) (midpoint-segment (make-segment (make-point 0 0) (make-point 3 3))))
        "Procedure #midpoint-segment works as expected.")

; {{{2 Exercise 2.3:
; {{{3 Problem
;      Implement a representation for rectangles in a plane.  (Hint: You
;      may want to make use of Exercise 2-2.)  In terms of your
;      constructors and selectors, create procedures that compute the
;      perimeter and the area of a given rectangle.  Now implement a
;      different representation for rectangles.  Can you design your
;      system with suitable abstraction barriers, so that the same
;      perimeter and area procedures will work using either
;      representation?
;
; {{{3 Solution
;      At it's most basic a rectangle may be represented by either two points
;      that represent vertices at opposite diagonal corners, or by any vertex
;      and two integers, one representing the height and the other the length.
;
;      In order to be able to use the same area and perimeter procedure for
;      either of these representations, we would have to define them in terms
;      of the height and length of the rectangle and provide selectors that
;      return height or length given a rectangle.
;
(define (area-rec rectangle)
  (* (height-rec rectangle)
     (width-rec rectangle)))

(define (perimeter-rec rectangle)
  (+ (* 2 (height-rec rectangle))
     (* 2 (width-rec rectangle))))

(define (make-rec vertex height width)
  ; This is iteresting because not only can a rectangle be represented in
  ; different ways but also the data can be arranged differenlty depending on
  ; the need. For example, In this case I have several choices, among them are
  ; where and how to arrange the vertex, height and width.  I could either
  ; arrange them so that the height and the width come first and thus optimize
  ; for use cases where I would need to get the height and width.  On the other
  ; hand, I could make the vertex point come first, thus optimize for use
  ; cases where needing to know the position of the rectangles on a grid is
  ; important.
  (let ((hw-point (cons height width)))
    (cons vertex hw-point)))

(define (height-rec rectangle)
  (car (cdr rectangle)))

(define (width-rec rectangle)
  (cdr (cdr rectangle)))

(assert (= 20 (area-rec (make-rec (make-point 0 0) 5 4)))
        "Procedure #area-rec works as expected.")

(assert (= 5 (height-rec (make-rec (make-point 0 0) 5 4)))
        "Procedure #height-rec works as expected.")

(assert (= 4 (width-rec (make-rec (make-point 0 0) 5 4)))
        "Procedure #width-rec works as expected.")

(define (make-rec vertex1 vertex2)
  (cons vertex1 vertex2))

(define (height-rec rectangle)
  (abs (- (x-point (car rectangle))
          (x-point (cdr rectangle)))))

(define (height-width rectangle)
  (abs (- (y-point (car rectangle))
          (y-point (cdr rectangle)))))

(assert (= 20 (area-rec (make-rec (make-point 0 0) (make-point 5 4))))
        "Procedure #area-rec works as expected.")

(assert (= 5 (height-rec (make-rec (make-point 0 0) (make-point 5 4))))
        "Procedure #height-rec works as expected.")

(assert (= 4 (width-rec (make-rec (make-point 0 0) (make-point 5 4))))
        "Procedure #width-rec works as expected.")

; {{{1 2.1.3 What Is Meant by Data? (2.4 - 2.6)
; {{{2 Exercise 2.4:
; {{{3 Problem
;      Here is an alternative procedural representation of pairs.  For this
;      representation, verify that =(car (cons x y))= yields =x= for any
;      objects =x= and =y=.

          (define (cons x y)
            ; Returns a λ function that takes one argument and applies it to x and y.
            (lambda (m) (m x y)))

          (define (car z)
            (z (lambda (p q) p)))

;      What is the corresponding definition of =cdr=? (Hint: To verify that
;      this works, make use of the substitution model of section 1.1.5.)
;
; {{{3 Solution
;
; First we  verify that the above #cons and  #car procedure work as expected.
; (cons 1 2)
; (λ (m) 1 2)
; (car (λ (m) 1 2))
; ((λ (m) (m 1 2) (λ (p q) p))
; (λ (1 2) p)
; 1
(assert (= 1 (car (cons 1 2)))
        "Verity that procedural #cons and #car work as expected.")

          (define (cdr z)
            ; The corresponding cdr implementation would simply return q.
            (z (lambda (p q) q)))

; (λ (m) 1 2)
; (car (λ (m) 1 2))
; ((λ (m) (m 1 2) (λ (p q) p))
; (λ (1 2) q)
; 2
(assert (= 2 (cdr (cons 1 2)))
        "Verity that procedural #cdr works as expected.")

; {{{2 Exercise 2.5:
;
; {{{3 Problem
;      Show that we can represent pairs of non-negative integers using
;      only numbers and arithmetic operations if we represent the pair
;      `a` and `b` as the integer that is the product `2^a 3^b`.  Give
;      the corresponding definitions of the procedures #cons, #car,
;      and #cdr.
;
; {{{3 Solution
;
;      Notice that we can divide any number of type `2^a 3^b` recursively by
;      2 until the point where the  division will no longer result in an
;      integer.  Thus, this provides a way of retrieving `a` from the number
;      and a complementary  procedure can be applied to retrieve `b`
(define (cons a b)
  (* (pow 2 a)
     (pow 3 b)))

(define (car x )
  (if (not (= 0 (modulo x 2)))
    0
    (+ 1 (car (/ x 2)))))

(define (cdr x )
  (if (not (= 0 (modulo x 3)))
    0
    (+ 1 (cdr (/ x 3)))))

(assert (= 7 (car (cons 7  12)))
        "Verity that arithmetic #car and #cons  work as expected.")

(assert (= 12 (cdr (cons 7  12)))
        "Verity that arithmetic #cdr works as expected.")

; {{{2 Exercise 2.6:
;
; {{{3 Problem
;
;      In case representing pairs as procedures wasn't mind-boggling
;      enough, consider that, in a language that can manipulate
;      procedures, we can get by without numbers (at least insofar as
;      non-negative integers are concerned) by implementing 0 and the
;      operation of adding 1 as

          (define zero
            ; Define zero as a λ function that when passed a function `f`
            ; returns another λ function that takes any attribute and returns
            ; the same attribute back.  In effect, we are nullifying `f`.  (We
            ; can see why this may be an apt definition for zero in this
            ; system).
            (lambda (f) (lambda (x) x)))

          (define (add-1 n)
            ; Although a bit more difficult to visualize, #add-1 will create
            ; a function that essentially composes `f` unto the return value of
            ; the inner λ function returned by any number in this system.  For
            ; example applying #add-1 to `zero` results in:
            ;
            ; (λ (f) (λ (x) (f x)))
            ;
            ; Note that where `zero` returned `x`, `one-p` returns `(f x)`
            ; (The above result is verified in the solution section).
            (lambda (f) (lambda (x) (f ((n f) x)))))

;      This representation is known as "Church numerals", after its
;      inventor, Alonzo Church, the logician who invented the λ
;      calculus.
;
;      Define `one` and `two` directly (not in terms of `zero` and
;      #add-1).  (Hint: Use substitution to evaluate `(add-1 zero)`).
;      Give a direct definition of the addition procedure `+' (not in
;      terms of repeated application of `add-1').
;
; {{{3 Solution
;
; Using the substitution method, we can evaluate `(add-1 zero)`:
;
; (add-1 zero)
; ((λ (f) (λ (x) (f ((n f) x)))) (λ (f) (λ (x) x)))
; ((λ (f) (λ (x) (f (((λ (f) (λ (x) x)) f) x)))))
; ((λ (f) (λ (x) (f ((λ (x) x) x)))))
; (λ (f) (λ (x) (f x)))
;
; Thus,
(define one
  ; Comparing `one` to `zero` we see that the effect of `f` on the final
  ; function is to be applied once instead of zero times.
  (lambda (f) (lambda (x) (f x))))
;
; Next we derive `two` in this system by applying the substitution method to
; `(add-1 one)`:
;
; (add-1 one)
; ((λ (f) (λ (x) (f ((n f) x)))) (λ (f) (λ (x) (f x))))
; (λ (f) (λ (x) (f (((λ (f) (λ (x) (f x))) f) x))))
; (λ (f) (λ (x) (f ((λ (x) (f x)) x))))
; (λ (f) (λ (x) (f (f x))))
;
; Thus,
(define two
  ; Comparing `two` to `zero` we see that the effect of `f` on the final
  ; function is to be applied twice instead of zero times.  Or, comparing `two`
  ; to `one` we see the effect of `f` is to be applied one more time than in
  ; `one`.
  (lambda (f) (lambda (x) (f (f x)))))

; Let's assert that our derived definition for `one` and `two` are correct.
;
; To make these assertions, we need to compare the substitution method derived
; `one` and `two` to the corresponding procedurally derived `one-p` and
; `two-p`. Also because we cannot directly compare these procedures, at this
; point, we will compare the results of applying `one`, `one-p`, `two` and
; `two-p` to a function `f`,  where `f` is a λ function that adds 1 to a passed
; in parameter `x`:

(define (add-one)
  (lambda (x) (+ 1 x)))

(define f add-one)

(define one-p (add-1 zero))

(define two-p (add-1 one-p))

(assert (= 1 ((one (f)) 0) ((one-p (f)) 0))
        "The substitution method derived `one` is equivalent to the procedurally derived `one-p`.")
(assert (= 2 ((two (f)) 0) ((two-p (f)) 0))
        "The substitution method derived `two` is equivalent to the procedurally derived `two-p`.")
(assert (= 0 ((zero (f)) 0))
        "`zero` nullifies the effect of #f.")

; The effect of #add-1 on `one-p` was the composition of `f` onto `one-p`.
;
; one : (λ (f) (λ (x) (f x)))
; two : (λ (f) (λ (x) (f (f x))))
;
; Thus one may surmise from this that the procedure that adds the numbers in
; this system are  somehow composed together from the specific numbers.

(define (adder a b)
  ; My naive attempt at #adder was the direct composition of `a` and `b`, but
  ; that clearly was wrong. Some trial and error led me to the correct
  ; solution.
  (lambda (f) (compose (a f) (b f))))

(define three (adder one two))
(define three-r (adder two one))
(assert (= 3 ((three (f)) 0) ((three-r (f)) 0))
        "The #adder procedure can add numbers in our system..")

(define four (adder one three))
(define four-r (adder three one))
(define four-2 (adder two two))
(assert (= 4 ((four (f)) 0) ((four-r (f)) 0) ((four-2 (f)) 0))
        "The #adder procedure can add numbers in our system..")

; There are four properties of addition in the regular numerical system:
; communicative, associative, additive identity and distributive.  The
; following assertions will test the first three of these properties. Since the
; distributive property requires a multiplication operator, we will
; skip testing that property.

(assert (= (((adder two one) (f)) 0)  (((adder one two) (f)) 0))
        "#adder has communicative property because the order of `a` and `b`
        does not matter.")
(assert (= (((adder (adder three two) one) (f)) 0)  (((adder one (adder two three)) (f)) 0))
        "#adder has associative property because
        `(adder (adder one two) three)` is the same as
        `(adder one (adder two three)`.")
(assert (= ((four (f)) 0) (((adder zero four) (f)) 0) )
        "#adder has additive identity property because `(adder zero number)` is
        `number`.")
