
(load '("lib/assert"
        "lib/math"
        "lib/picture/pic-imag"
        "lib/picture/pic-ops"
        "lib/picture/pic-read"
        "lib/picture/pic-reco"
        "lib/picture/picture"
        "lib/picture/prmpnt"
        "lib/picture/hutils"
        "lib/picture/hend.scm"))

; {{{1 2.2.4 Example: A Picture Language
;
; {{{2 Exercise 2.44:
; {{{3 Problem
;      Define the procedure `up-split' used by
;      `corner-split'.  It is similar to `right-split', except that it
;      switches the roles of `below' and `beside'.
;
; {{{3 Solution

(define flip-vert
  (compose rotate180 flip-horiz))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

; {{{2 Exercise 2.45:
; {{{3 Problem
;      `Right-split' and `up-split' can be expressed as
;      instances of a general splitting operation.  Define a procedure
;      `split' with the property that evaluating
;
;           (define right-split (split beside below))
;           (define up-split (split below beside))
;
;      produces procedures `right-split' and `up-split' with the same
;      behaviors as the ones already defined.
;
; {{{3 Solution
(define (split min-sider max-sider)
  (lambda (painter n)
    (define (*split painter n)
      (if (= n 0)
        painter
        (let ((smaller (*split painter (- n 1))))
          (min-sider painter (max-sider smaller smaller)))))
    (*split painter n)))

(define right-split (split beside below))
(define up-split (split below beside))
; {{{2 Exercise 2.46:
; {{{3 Problem
;      A two-dimensional vector v running from the
;      origin to a point can be represented as a pair consisting of an
;      x-coordinate and a y-coordinate.  Implement a data abstraction for
;      vectors by giving a constructor `make-vect' and corresponding
;      selectors `xcor-vect' and `ycor-vect'.  In terms of your selectors
;      and constructor, implement procedures `add-vect', `sub-vect', and
;      `scale-vect' that perform the operations vector addition, vector
;      subtraction, and multiplying a vector by a scalar:
;
;           (x_1, y_1) + (x_2, y_2) = (x_1 + x_2, y_1 + y_2)
;           (x_1, y_1) - (x_2, y_2) = (x_1 - x_2, y_1 - y_2)
;                        s * (x, y) = (sx, sy)
;
; {{{3 Solution
(define (make-vect x y)
  (list 'vector x y))

(define vector-xcor cadr)

(define vector-ycor caddr)

(define (vector-scale s v)
  (make-vect (* s (vector-xcor v))
             (* s (vector-ycor v))))

(define (vector-add v1 v2)
  (make-vect (+ (vector-xcor v1) (vector-xcor v2))
             (+ (vector-ycor v1) (vector-ycor v2))))

(define (vector-sub v1 v2)
  (make-vect (- (vector-xcor v1) (vector-xcor v2))
             (- (vector-ycor v1) (vector-ycor v2))))

(assert (equal? (list 'vector 1 2) (make-vect 1 2)) "(make-vect 1 2)")

(assert (= 1 (vector-xcor (make-vect 1 2))) "(vector-xcor [1 2])")

(let ((expected (list 'vector 2 3))
      (actual (vector-add (make-vect 1 2) (make-vect 1 1))))
  (assert (equal? expected actual) "(vector-add [1 2] [1 1])"))

(let ((expected (list 'vector 0 1))
      (actual (vector-sub (make-vect 1 2) (make-vect 1 1))))
  (assert (equal? expected actual) "(vector-sub [1 2] [1 1])"))

(let ((expected (list 'vector 2 4))
      (actual (vector-scale 2 (make-vect 1 2))))
  (assert (equal? expected actual) "(vector-scale 2 [2 4])"))

; {{{2 Exercise 2.47:
; {{{3 Problem
;      Here are two possible constructors for frames:


(define (make-frame origin edge1 edge2)
  (list 'frame origin edge1 edge2))
(define frame-origin cadr)
(define frame-edge1 caddr)
(define frame-edge2 cadddr)

(define test-org (make-vect 0 1))
(define test-edg1 (make-vect 1 1))
(define test-edg2 (make-vect 1 2))
(define test-frame (make-frame test-org test-edg1 test-edg2))

(assert (equal? test-org (frame-origin test-frame)) "(frame-orgin frame)")
(assert (equal? test-edg1 (frame-edge1 test-frame)) "(frame-edge1 frame)")
(assert (equal? test-edg2 (frame-edge2 test-frame)) "(frame-edge2 frame)")

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define frame-origin car)
(define frame-edge1 cadr)
(define frame-edge2 cddr)

(define test-org (make-vect 0 1))
(define test-edg1 (make-vect 1 1))
(define test-edg2 (make-vect 1 2))
(define test-frame (make-frame test-org test-edg1 test-edg2))

(assert (equal? test-org (frame-origin test-frame)) "(frame-orgin frame)")
(assert (equal? test-edg1 (frame-edge1 test-frame)) "(frame-edge1 frame)")
(assert (equal? test-edg2 (frame-edge2 test-frame)) "(frame-edge2 frame)")

;
;      For each constructor supply the appropriate selectors to produce an
;      implementation for frames.
;
; {{{3 Solution
; {{{2 Exercise 2.48:
; {{{3 Problem
;      A directed line segment in the plane can be
;      represented as a pair of vectors--the vector running from the
;      origin to the start-point of the segment, and the vector running
;      from the origin to the end-point of the segment.  Use your vector
;      representation from *Note Exercise 2-46:: to define a
;      representation for segments with a constructor `make-segment' and
;      selectors `start-segment' and `end-segment'.
;
; {{{3 Solution

(define (make-segment start end)
  (list 'segment start end))

(define segment-start cadr)

(define segment-end caddr)

(define test-start (make-vect 0 1))
(define test-end (make-vect 1 0))
(define test-segment (make-segment test-start test-end))

(assert (equal? test-start (segment-start test-segment)) "(segment-start segment)")
(assert (equal? test-end (segment-end test-segment)) "(segment-end segment)")


; {{{2 Exercise 2.49:
; {{{3 Problem
;      Use `segments->painter' to define the following
;      primitive painters:
;
;        a. The painter that draws the outline of the designated frame.
;
;        b. The painter that draws an "X" by connecting opposite corners
;           of the frame.
;
;        c. The painter that draws a diamond shape by connecting the
;           midpoints of the sides of the frame.
;
;        d. The `wave' painter.
;
; {{{3 Solution

(define outline
  (segments->painter
    (list (make-segment (make-vect .1 .1) (make-vect .1 .9))
          (make-segment (make-vect .1 .9) (make-vect .9 .9))
          (make-segment (make-vect .9 .9) (make-vect .9 .1))
          (make-segment (make-vect .9 .1) (make-vect .1 .1)))))

(define x
  (segments->painter
    (list (make-segment (make-vect .1 .1) (make-vect .9 .9))
          (make-segment (make-vect .1 .9) (make-vect .9 .1)))))

(define diamond
  (segments->painter
    (list (make-segment (make-vect .0 .5) (make-vect .5 1.))
          (make-segment (make-vect .5 1.) (make-vect 1. .5))
          (make-segment (make-vect 1. .5) (make-vect .5 .0))
          (make-segment (make-vect .5 0) (make-vect .0 .5)))))

(define wave
  ;; copied from library.
  (segments->painter
    (list (make-segment (make-vect 0.25 0.00) (make-vect 0.37 0.37)) ;1
          (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.25)) ;2
          (make-segment (make-vect 0.50 0.25) (make-vect 0.62 0.00)) ;3
          (make-segment (make-vect 0.75 0.00) (make-vect 0.70 0.50)) ;4
          (make-segment (make-vect 0.70 0.50) (make-vect 1.00 0.30)) ;5
          (make-segment (make-vect 1.00 0.50) (make-vect 0.75 0.62)) ;6
          (make-segment (make-vect 0.75 0.62) (make-vect 0.62 0.62)) ;7
          (make-segment (make-vect 0.62 0.62) (make-vect 0.75 0.75)) ;8
          (make-segment (make-vect 0.75 0.75) (make-vect 0.62 1.00)) ;9
          (make-segment (make-vect 0.40 1.00) (make-vect 0.30 0.75)) ;10
          (make-segment (make-vect 0.30 0.75) (make-vect 0.40 0.62)) ;11
          (make-segment (make-vect 0.40 0.62) (make-vect 0.25 0.62)) ;12
          (make-segment (make-vect 0.25 0.62) (make-vect 0.20 0.50)) ;13
          (make-segment (make-vect 0.20 0.50) (make-vect 0.00 0.70)) ;14
          (make-segment (make-vect 0.37 0.37) (make-vect 0.30 0.50)) ;15
          (make-segment (make-vect 0.30 0.50) (make-vect 0.12 0.37)) ;16
          (make-segment (make-vect 0.12 0.37) (make-vect 0.00 0.50)) ;17
          )))
; {{{2 Exercise 2.50:
; {{{3 Problem
;      Define the transformation `flip-horiz', which
;      flips painters horizontally, and transformations that rotate
;      painters counterclockwise by 180 degrees and 270 degrees.
;
; {{{3 Solution
; {{{2 Exercise 2.51:
; {{{3 Problem
;      Define the `below' operation for painters.
;      `Below' takes two painters as arguments.  The resulting painter,
;      given a frame, draws with the first painter in the bottom of the
;      frame and with the second painter in the top.  Define `below' in
;      two different ways--first by writing a procedure that is analogous
;      to the `beside' procedure given above, and again in terms of
;      `beside' and suitable rotation operations (from *Note Exercise
;      2-50::).
;
; {{{3 Solution
; {{{2 Exercise 2.52:
; {{{3 Problem
;      Make changes to the square limit of `wave' shown
;      in *Note Figure 2-9:: by working at each of the levels described
;      above.  In particular:
;
;        a. Add some segments to the primitive `wave' painter of *Note
;           Exercise 2-49:: (to add a smile, for example).
;
;        b. Change the pattern constructed by `corner-split' (for
;           example, by using only one copy of the `up-split' and
;           `right-split' images instead of two).
;
;        c. Modify the version of `square-limit' that uses
;           `square-of-four' so as to assemble the corners in a different
;           pattern.  (For example, you might make the big Mr. Rogers
;           look outward from each corner of the square.)
;
; {{{3 Solution
;
