; Ex 1.1
; 10
; 12
; 8
; 3 
; 6
; a is assigned to 3
; b is assigned to 4
; 19
; #f
; 4
; 16
; 6
; 16

; Ex 1.2
(/ 
  (+ 5 4
     (- 2
        (- 3
           (+ 6
              (/ 4 5)))))
  (* 3
     (- 6 2)
     (- 2 7)))

; Ex 1.3
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (minimum x y) (if (< x y) x y))
(define (maximum x y) (if (> x y) x y))
(define (sum-squares-two-largest x y z)
  (sum-of-squares (maximum x y) (maximum z (minimum x y))))

; Ex 1.4
; Example 1
; (define a 1) (define b 1)
; 2
; Example 2
; (define a 1) (define b -1)
; 2
; In step 2, (= (< b 0) #t), so the - operator was applied to the operands (a b) giving (- 1 -1) -> 1 - -1 -> 1 + 1 -> 2

; Ex 1.5
; Applicative order will reduce (p) to (p) to (p) to ... infinitely recursively
; Normal order will pass (p) to test unevaluated, since it only evaluates when it needs to, and thus test will return 0 in this case.

; ----------------------------------------------------------------------------
; Square root computation
(define (good-enough? guess x) (> 0.001 (abs (- (square guess) x))))
(define (avg x y) (/ (+ x y) 2))
(define (improve guess x) (avg guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))
(define (squarert x)
  (sqrt-iter 1.0 x))
; ----------------------------------------------------------------------------

; Ex 1.6
; The special form (if ..) is evaluated in a special way - first the predicate, then the consequent, XOR the alternative. The function new-if is evaluated by applicative order, which reduces (sqrt-iter ...) to (sqrt-iter ...) to (sqrt-iter ...) to ... infinitely recursively.

; Ex 1.7
(define (good-enough? guess previous-guess) (> 0.001 (abs (- guess previous-guess))))

(define (sqrt-iter guess previous-guess x)
  (if (good-enough? guess previous-guess)
    guess
    (sqrt-iter (improve guess x)
               guess
               x)))
(define (squarert x)
  (sqrt-iter 1.0 0 x))

; Ex 1.8
(define (cube-improve guess x)
  (/ 
    (+
      (/ x (square guess))
      (* 2 guess))
    3))

(define (cbrt-iter guess previous-guess x)
  (if (good-enough? guess previous-guess)
    guess
    (cube-iter (cbrt-improve guess x)
               guess
               x)))
(define (cubert x)
  (cube-iter 1.0 0 x))

; ----------------------------------------------------------------------------
; Lexically scoped square root and helper functions
(define (squarert x)
  (define (good-enough? guess previous-guess) 
    (> 0.001 
       (abs (- guess previous-guess))))
  (define (avg a b) (/ (+ a b) 2))
  (define (improve guess) (avg guess (/ x guess)))
  (define (sqrt-iter guess previous-guess)
    (if (good-enough? guess previous-guess)
      guess
      (sqrt-iter (improve guess) guess)))
  (sqrt-iter 1.0 0))
; ----------------------------------------------------------------------------
; An iterative process described by a recursive proceedure (tail-recursive) to calculate factorial n
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product) (+ counter 1))))
  (iter 1 1))
; ----------------------------------------------------------------------------

; Ex 1.9
; (inc (+ (dec 4) 5))
; (inc (inc (+ (dec 3) 5)))
; (inc (inc (inc (+ (dec 2) 5))))
; (inc (inc (inc (inc (+ (dec 1) 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
; this is a recursive process
; (+ (dec 4) (inc 5))
; (+ (dec 3) (inc 6))
; (+ (dec 2) (inc 7))
; (+ (dec 1) (inc 8))
; 9
; this is an iterative process

; Ex 1.10
(define (A x y)
  (cond
    ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else (A (- x 1)
             (A x (- y 1))))))
; 1024
; 65536
; 65536
; (f n) computes 2n
; (g n) computes 2^n for n > 0. 0 otherwise.
; (h n) computes 2^^n for n > 0. 0 otherwise. For example (h 3) computes 2^2^2, (h 4) computes 2^2^2^2.

; Ex 1.11
; fibonacci
; 0 1 1 2 3 5 8 13 ...
; a <- a + b
; b <- a
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (fib n)
  (define (iter a b count)
    (if (= count 0)
      b
      (iter (+ a b) a (- count 1)))))
; f
; 0 1 2 4 11 25 59 ...
; a <- a + 2b + 3c
; b <- a
; c <- b
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))
(define (f n)
  (define (iter a b c count)
   (if (< count 3)
     a
     (iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (if (< n 3)
    n
  (iter 2 1 0 n)))
; Ex 1.12
;       1       <-- row 0, col 0
;     1   1
;   1   2   1
; 1   3   3   1
;     ^
;     |
; row 3, col 1
; To compute number 3 labeled here, we do (pascal 3 1), which is (+ (pascal 2 1) (pascal 2 0)), which is (+ (+ (pascal 1 1) (pascal 1 0)) 1), which is (+ (+ 1 1) 1), which is 3
(define (pascal row col)
  (cond
    ((or (= col 0) (= col row)) 1)
    (else (+ (pascal (- row 1) col)
             (pascal (- row 1) (- col 1))))))

; Ex 1.13 is.. hard.
; I understand the proof here http://www.kendyck.com/math/sicp/ex1-13.xml but could not have thought of it myself :(

(define (count-change amount)
  (define (first-denomination kind-of-coin)
    (cond ((= kind-of-coin 1) 1)
          ((= kind-of-coin 2) 5)
          ((= kind-of-coin 3) 10)
          ((= kind-of-coin 4) 25)
          ((= kind-of-coin 5) 50)))
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount (- kinds-of-coins 1))
                   (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))
  (cc amount 5))

;(define (fast-expt b n)
;  (
