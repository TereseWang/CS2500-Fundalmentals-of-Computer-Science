;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname HW-12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Exercise 1
;x: Number Number -> Number
;add two numbers together
(check-expect (x 1 2) 3)
(check-expect (x 44 5) 49)
(check-expect (x -5 8) 3)
(define (x y z)
  (local [;x: Number -> Number
          ;add the value number to the given number
          ;If y=4, given 3, it should produces 7
          (define (x z) (+ z y))]
    (x z)))

;Exercise 2
;a: [Listof Number] Number Number -> Number
(check-expect (a (list 1 2 3) 2 3) 88)
(check-expect (a (list 1 2 3) 4 5) 180)
(check-expect (a (list 1 2 3) -2 3) 28)
(check-expect (a empty 2 3) 15)
(define (a b c d)
  (local [(define e (+ d c))]
    (cond [(empty? b) (* e d)]
          [(cons? b) (a (rest b) (first b) e)])))

;Exercise 3
;First, Check-expects can not check the equivalence of the two functions
;because in order to do this DrRacket would have to check an infinite
;number of inputs for the functions to determine if they always produced the same result.
;Second, the local function s expects two inputs, however, q can only take one
;so when called s cannot be evaluated so q will not work.

;Exercise 4
;adder: Number -> [Number -> Number]
;provide the function that add a number to the given number
(check-expect (test-helper (adder 0) 2) 2)
(check-expect (test-helper (adder -6) -3) -9)
(check-expect (test-helper (adder 5) 5) 10)
(define (adder x)
  (local [;f: Number -> Number
          ;add the value number to the given number
          (define (f z) (+ x z))] f))
;test-helper: [Number -> Number] Number -> Number
;Applies the function on the number
(check-expect (test-helper (adder 0) 2) 2)
(check-expect (test-helper (adder -6) 1) -5)
(define (test-helper func y)
  (func y))

; Exercise 5:

; A ListOfPosns is one of:
; - '()
; - (cons Posn ListOfPosns)
; and represents a list of positions on the x-y plane
 
(define LOP-EMPTY '())
(define LOP-NON-EMPTY (cons (make-posn 8 6) (cons (make-posn 1 2) '())))
 
; sum-x-coords : ListOfPosns -> Number
; Sum all the x-coordinates in the list of positions
(check-expect (sum-x-coords LOP-EMPTY) 0)
(check-expect (sum-x-coords LOP-NON-EMPTY) 9)
(define (sum-x-coords lop)
  (process-lop + posn-x 0 lop))
 
; mult-distances : ListOfPosns -> Number
; Multiply all the distances from each position to the origin
(check-expect (mult-distances LOP-EMPTY) 1)
(check-within (mult-distances LOP-NON-EMPTY) (* 10 (sqrt 5)) 1e-06)
(define (mult-distances lop)
  (process-lop * distance-to-origin 1 lop))
 
; distance-to-origin : Posn -> Number
; Produces the distance from this position to the origin
(check-expect (distance-to-origin (make-posn 3 4)) 5)
(check-within (distance-to-origin (make-posn 10 5)) (sqrt 125) 1e-06)
(define (distance-to-origin p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

; process-lop: [Number -> Number] [Posn -> Number] Number ListOfPosns -> Number
; Applies the given function on each Posn and returns the
; result of the performed operation on every number
(check-expect (process-lop + posn-x 0 LOP-NON-EMPTY) 9)
(check-within (process-lop * distance-to-origin 1 LOP-NON-EMPTY) (* 10 (sqrt 5)) 1e-06)
(check-expect (process-lop + posn-y 0 LOP-NON-EMPTY) 8)
(define (process-lop oper func base lop)
  (cond [(empty? lop) base]
        [(cons? lop) (oper (func (first lop))
                           (process-lop oper func base (rest lop)))]))

; Exercise 6:

;sum-ratios: ListOfPosns -> Number
;Adds all of the ratios of each posn together
(check-expect (sum-ratios '()) 0)
(check-expect (sum-ratios (cons (make-posn 10 5) '())) (/ 10 5))
(check-expect (sum-ratios (cons (make-posn 10 5) (cons (make-posn 2 3) '())))
              (+ (/ 10 5) (/ 2 3)))
(define (sum-ratios lop)
  (process-lop + posn-ratio 0 lop))

;posn-ratio: Posn -> Number
;Returns ratio of posn-x to posn-y
(check-expect (posn-ratio (make-posn 10 5)) 2)
(check-expect (posn-ratio (make-posn 2 4)) .5)
(define (posn-ratio posn)
  (/ (posn-x posn) (posn-y posn)))