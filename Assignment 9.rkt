;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Assignment 9(new)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define FIXING-STYLE-ISSUES #true)

;Exercise 1
(define-struct plist [frst rst])
;A PseudoLON is one of:
;- "empty"
;- (make-plist Number PseudoLON)

(define P1 "empty")
(define P2 (make-plist 1 "empty"))
(define P3 (make-plist 2 P2))
(define P4 (make-plist 3 P3))

;plist-temp: PseudoLON -> ???
#;(define (plist-temp p)
    (cond [(string? p) ... ]
          [else ... (plist-frst p)
                ... (plist-temp (plist-rst p)) ...]))

;Exercise 2

;add-up-all: PseudoLON -> Number
;add up all the numbers in the PseudoLON

(check-expect (add-up-all P1) 0)
(check-expect (add-up-all P2) 1)
(check-expect (add-up-all P3) 3)
(check-expect (add-up-all P4) 6)

(define (add-up-all plon)
  (cond [(string? plon) 0]
        [(plist? plon) (+ (plist-frst plon) (add-up-all (plist-rst plon)))]))

;Exercise 3

;The code will break, since the first cases of string? are not tested,
;which means all questions are false as a result.
;The reason behind the break is because PLIST1 does not include an "empty",
;when the data definition says a PseudoLon is one of "empty"
;and (make-plist Number PseudoLON).
;Besides, since the add-up-all is a recursive function, it will be back
;to its base case "empty", when the function does not include the base
;case in the recursive function, it will blow up. 

;Exercise 4

;A LON is one of:
;- '()
;- (cons Number LON)
;Interpretation: A '() is an empty list with no items listed
;The list with the given Number at the front,
;then all the items in the recursive list.

(define L1 '())
(define L2 (cons 1 L1))
(define L3 (cons 2 L2))
(define L4 (cons 3 L3))

;lon-temp: LON -> ???
#;(define (lon-temp lon)
    (cond [(empty? lon) ...]
          [else ... (first lon)
                ... (lon-temp (rest lon)) ...]))


;Exercise 5
;add-lon: LON -> Number
;add all the numbers up in the LoN

(check-expect (add-lon L1) 0)
(check-expect (add-lon L2) 1)
(check-expect (add-lon L3) 3)
(check-expect (add-lon L4) 6)

(define (add-lon lon)
  (cond [(empty? lon) 0]
        [(cons? lon) (+ (first lon) (add-lon (rest lon)))]))

;Exercise 6

;The code will break, since the first cases of empty? are not tested,
;which means all questions are false as a result.
;The reason behind the break is because LIST1 does not include an empty list,
;when the data definition says a LON is one of '()
;Besides, since the add-up-all is a recursive function, it will be back
;to its base case '(), when the function does not include the base
;case in the recursive function, it will blow up. 

;Exercise 7

;A ListOfStrings (LOS) is one of
;- '()
;- (cons AlphabeticString LOS)
;Interpretation 
;Where '() is the empty list with no items listed

;An AlphabeticString is the strings that contains only letters 
;The list with the given string at the front,
;then all the items in the recursive list.

(define LS0 '())
(define LS1 (cons "apple" (cons "banana" '())))
(define LS2 (cons "banana" (cons "apple" '())))
(define LS3 (cons "apple" (cons "Banana" (cons "orange" '()))))
(define LS4 (cons "orange" (cons "Banana" (cons "orange" '()))))

;los-temp: LOS -> ???
#;(define (los-temp los)
    (cond [(empty? los) ...]
          [else ... (first los)
                ... (los-temp (rest los)) ...]))

;all-in-order?: LOS -> Boolean
;Are the given LOS (list of strings) in alphabetic orders?

(check-expect (all-in-order? LS1) #true)
(check-expect (all-in-order? LS0) #true)
(check-expect (all-in-order? LS2) #false)
(check-expect (all-in-order? LS3) #true)
(define (all-in-order? los)
  (cond [(empty? los) #true]
        [else (and (in-order? (first los) (rest los))
                   (all-in-order? (rest los)))]))

;in-order?: String LOS -> Boolean
;Are the string and the first of the LoS in alphabetic orders  

(check-expect (in-order? "apple" (cons "apple" '())) #true)
(check-expect (in-order? "apple" (cons "apple" (cons "orange" (cons "apple" '())))) #true)
(check-expect (in-order? "zoo" (cons "orange" (cons "apple" '()))) #false)
(define (in-order? string1 rest)
  (cond [(empty? rest) #true]
        [else (string-ci<=? string1 (first rest))]))

;Exercise 8

(require 2htdp/image)

(define I1 (circle 10 "solid" "red"))
(define I2 (rectangle 20 30 "solid" "blue"))
(define I3 (triangle 40 "solid" "tan"))

;An ListOfImagesI (LOI) is one of
;- '()
;- (cons Image LOI)
;Interpretation: an '() is an empty list with no items listed
;The list with the given Image at the front,
;then all the items in the recursive list.

(define LOI0 '())
(define LOI1 (cons I1 '()))
(define LOI2 (cons I2 (cons I1 '())))

;smoosh: LOI -> Image
;place the images next to each others in order
;which the front of the list appear at the left

(check-expect (smoosh LOI0) empty-image)
(check-expect (smoosh LOI1) (beside I1 empty-image))
(check-expect (smoosh LOI2) (beside I2 (beside I1 empty-image)))

(define (smoosh loi)
  (cond [(empty? loi) empty-image]
        [else (beside (first loi) (smoosh (rest loi)))]))

;Exercise 9

;A ListOfNumbers (or LON) is one of:
;- '() 
;- (cons Number LON)
;Interpretation: An '() is an empty list with no items listed
;The list with the given Number at the front,
;then all the items in the recursive list.

(define LON0 '())
(define LON1 (cons 9 (cons -4 (cons 100 '()))))
(define LON2 (cons 3 (cons 16/4 (cons 50 '()))))
(define LON3 (cons 1 (cons 2 (cons 3 LON0))))
(define LON4 (cons 1 (cons 2 (cons 3 (cons 4 LON0)))))
(define LON5 (cons 1 (cons 2 (cons 2 (cons 3 (cons 3 LON0))))))

;lon-temp: LON -> ???
#;(define (lon-temp lon)
    (cond [(empty? lon) ...]
          [else ... (first lon)
                ... (lon-temp (rest lon)) ...]))

;root-the-squares: LON -> LON
;returns the list of square roots of all the perfect squares

(check-expect (root-the-squares LON0) '())
(check-expect (root-the-squares LON1) (cons 3 (cons 10 '())))
(check-expect (root-the-squares LON2) (cons 2 '()))

(define (root-the-squares lon)
  (cond [(empty? lon) '()]
        [(and (positive? (first lon)) (integer? (sqrt (first lon))))
         (cons (sqrt (first lon)) (root-the-squares (rest lon)))]
        [else (root-the-squares (rest lon))]))

;Exercise 10

;set=?: LON LON -> Boolean
;determines if two LONs contain the exactly the same values

(check-expect (set=? LON4 LON4) #true)
(check-expect (set=? LON3 LON5) #true)
(check-expect (set=? LON0 LON5) #false)
(check-expect (set=? LON4 LON5) #false)
(check-expect (set=? LON0 '()) #true)

(define (set=? l1 l2)
  (and (in-set? l1 l2)(in-set? l2 l1)))

;in-set?: LON LON -> Boolean
;determines if all values in a LON are members of another LoN

(check-expect (in-set? LON3 LON5) #true)
(check-expect (in-set? LON5 LON3) #true)
(check-expect (in-set? LON5 LON5) #true)
(check-expect (in-set? LON5 LON4) #true)
(check-expect (in-set? LON4 LON5) #false)
(check-expect (in-set? LON0 LON3) #true)
(check-expect (in-set? LON3 LON0) #false)

(define (in-set? x y)
  (cond
    [(empty? x) #true]
    [(member (first x) y)(in-set? (rest x) y)]
    [else #false]))