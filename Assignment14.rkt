;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A ChurchBoolean is a function [X X -> X]
; Which represents a boolean
 
; church-true : ChurchBoolean
; Returns the first argument (represents #true)
(check-expect (church-true 3 4) 3)
(check-expect (church-true 2 1) 2)
(define (church-true x y) x)
 
; church-false : ChurchBoolean
; Returns the second argument (represents #false)
(check-expect (church-false 3 4) 4)
(check-expect (church-false 2 1) 1)
(define (church-false x y) y)

;Exercise 1:
;church->boolean: ChurchBoolean -> Boolean
;produces the actual boolean value the given ChurchBoolean represent
(check-expect (church->boolean church-false) #f)
(check-expect (church->boolean church-true) #t)
(define (church->boolean cb)
  (cb #t #f))

;Exercise 2:
;church-and: ChurchBoolean ChurchBoolean -> ChurchBoolean
;Are both given ChurchBooleans represent true? 
(check-expect (church->boolean (church-and church-true  church-true)) #t)
(check-expect (church->boolean (church-and church-false church-true)) #f)
(check-expect (church->boolean (church-and church-true  church-false)) #f)
(check-expect (church->boolean (church-and church-false church-false)) #f)
(define (church-and cb1 cb2)
  (λ (x y) (cb1 (cb2 x y) y)))

;Exercise 3:
;church-or: ChurchBoolean ChurchBoolean -> ChurchBoolean
;Is one of the ChurchBoolean represent true? 
(check-expect (church->boolean (church-or church-true  church-true)) #t)
(check-expect (church->boolean (church-or church-false church-true)) #t)
(check-expect (church->boolean (church-or church-true  church-false)) #t)
(check-expect (church->boolean (church-or church-false church-false)) #f)
(define (church-or cb1 cb2)
  (λ (x y) (cb1 x (cb2 x y))))

;Exercise 4:
;church-not:ChurchBoolean -> ChurchBoolean
;return the opposite of the given ChurchBoolean 
(check-expect (church->boolean (church-not church-true)) #f)
(check-expect (church->boolean (church-not church-false)) #t)
(define (church-not cb)
  (λ (x y) (cb y x)))

;Exercise 5:
;parity-matrix: Nat -> [Listof [Listof Booleans]]
;Produces a list of lists of booleans
;Is column i plus row j even?
(check-expect (parity-matrix 2) (list (list #t #f) (list #f #t)))
(check-expect (parity-matrix 3) (list (list #t #f #t)
                                      (list #f #t #f)
                                      (list #t #f #t)))
(check-expect (parity-matrix 4) (list (list #t #f #t #f)
                                      (list #f #t #f #t)
                                      (list #t #f #t #f)
                                      (list #f #t #f #t)))      
(define (parity-matrix n)
  (build-list n (λ (c) (build-list n (λ (r) (if (even? (+ c r)) #t #f))))))

;Exercise 6:
;cartesian-product: List List -> [Listof List]
;returns a list of all possible pairs of elements where the first element comes from the first
;given list and second element comes from the second 
(check-expect (cartesian-product (list "a" "b" "c") (list 1 2))
              (list (list "a" 1) (list "a" 2) (list "b" 1) (list "b" 2) (list "c" 1) (list "c" 2)))
(check-expect (cartesian-product (list 1 2) (list #t #f))
              (list (list 1 #t) (list 1 #f) (list 2 #t) (list 2 #f)))
(check-expect (cartesian-product (list "a") (list "a" "b"))
              (list (list "a" "a") (list "a" "b")))
(define (cartesian-product ls1 ls2)
  (build-list
   (* (length ls1) (length ls2))
   (λ (x) (list
           (list-ref ls1 (floor (/ x (length ls2))))
           (list-ref ls2 (modulo x (length ls2)))))))

;Exercise 7
;hypenate: [Listof String] [Listof String] -> [Listof String]
;Produces a list of strings, which string-append the first string of the first list and the second
;string of the second list with a "-" in between.
;Assume the two list have the same length 
(check-expect (hypenate (list "hello" "cool" "pet") (list "world" "oranges" "kind"))
              (list "hello-world" "cool-oranges" "pet-kind"))
(check-expect (hypenate (list "pe") (list "ople"))
              (list "pe-ople"))
(check-expect (hypenate '() '())
              '())
(define (hypenate ls1 ls2)
  (map (λ (s1 s2) (string-append s1 "-" s2)) ls1 ls2))

;Exercise 8:
(define-struct pet [name species age])
; A Pet is a (make-pet String String Nat)
; - where name is the pet's name
; - species is the pet's species
; - and age is the pet's age (in years)
(define PET-1 (make-pet "Sparky" "dog" 4))
(define PET-2 (make-pet "Sparky" "horse" 5))
(define PET-3 (make-pet "Fluffy" "dog" 4))
(define PET-4 (make-pet "Simon" "dog" 3))
(define PET-5 (make-pet "Tessy" "cat" 5))
(define LOP-0 (list PET-1 PET-2 PET-3))
(define LOP-1 (list PET-1 PET-3))
(define LOP-2 (list PET-1 PET-3 PET-4 PET-5))
;same-pet-name?: Pet Pet -> Boolean
;Does the given two pets have the same name? 
(check-expect (same-pet-name? PET-1 PET-2) #t)
(check-expect (same-pet-name? PET-2 PET-3) #f)
(define (same-pet-name? p1 p2)
  (string=? (pet-name p1) (pet-name p2)))

;Exercise 9:
;any-same-pet-name?: [Listof Pet] -> Boolean
;does any pets in the list share a name
(check-expect (any-same-pet-name? LOP-0) #t)
(check-expect (any-same-pet-name? LOP-1) #f)
(check-expect (any-same-pet-name? LOP-2) #f)
(define (any-same-pet-name? lop)
  (cond [(empty? lop) #false]
        [else (or (ormap (λ (x) (same-pet-name? (first lop) x)) (rest lop))
                  (any-same-pet-name? (rest lop)))]))
  
  
