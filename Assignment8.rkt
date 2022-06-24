;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Assignment8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define FIXING-STYLE-ISSUES #true)
;Exercise 1

(define-struct item [name price quantity])
; A GroceryItem is a (make-item String PositiveRealNumber Nat)
; - where name is the name of the item
; - price is the item's price at the nearest grocery store
; - and quantity is how many of the item you want to buy

;item-temp: GroceryItem -> ???
#;(define (item-temp i)
    (... (item-name i) ... (item-price i) ... (item-quantity i) ...))
(define IT1 (make-item "orange" 3 10))
(define IT2 (make-item "apple" 4 5))
(define IT3 (make-item "banana" 2 7))

(define-struct shopping [item others])
; A ShoppingList is one of:
; - "no items"
; - (make-shopping GroceryItem ShoppingList)
; and represents a list of items to buy at the grocery store

;shopping-temp : ShoppingList -> ???
#;(define (shopping-temp sp)
    (cond [(string? sp) ...]
          [(shopping? sp) (... (item-temp (shopping-item sp)) ...
                               (shopping-temp (shopping-others sp)) ...)]))
(define SP0 "no items")
(define SP1 (make-shopping (make-item "orange" 3 10) SP0))
(define SP2 (make-shopping (make-item "apple" 4 5) SP1))
(define SP3 (make-shopping (make-item "banana" 2 7) SP2))

;Exercise 2

;total-cost: ShoppingList -> PositiveRealNumber
;produces the total cost of purchasing everything on the list

(check-expect (total-cost SP0) 0)
(check-expect (total-cost SP1) 30)
(check-expect (total-cost SP2) 50)
(check-expect (total-cost SP3) 64)
(define (total-cost sp)
  (cond [(string? sp) 0]
        [(shopping? sp) (+ (* (item-price (shopping-item sp))
                              (item-quantity (shopping-item sp)))
                           (total-cost (shopping-others sp)))]))

;Exercise 3
(require 2htdp/image)

;Define the constants

(define COLOR "black")
(define FONTSIZE 20)
(define TITLE (text "Shopping List:" FONTSIZE "blue"))

;draw-image: ShoppingList -> Image
;draw "Shopping List:" above all the items listed
(check-expect (draw-image SP2) (above/align "left" TITLE (shoppinglist SP2)))
(check-expect (draw-image SP0) (above/align "left" TITLE (text "" FONTSIZE COLOR)))
(define (draw-image sp)
  (above/align "left" TITLE (shoppinglist sp)))

;shoppinglist: ShoppingList -> Image
;draw the front of listed items above each others
(check-expect (shoppinglist SP0) (text "" FONTSIZE COLOR))
(check-expect (shoppinglist SP1) (above/align "left"
                                              (draw-text (shopping-item SP1))
                                              (text "" FONTSIZE COLOR)))

(define (shoppinglist sp)
  (cond [(string? sp) (text "" FONTSIZE COLOR)]
        [(shopping? sp)
         (above/align "left"
                      (draw-text (shopping-item sp))
                      (shoppinglist (shopping-others sp)))]))

;draw-text: GroceryItem -> Image
;turns the item name into text images

(check-expect (draw-text IT1) (text "orange" FONTSIZE COLOR))
(check-expect (draw-text IT2) (text "apple" FONTSIZE COLOR))

(define (draw-text i)
  (text (item-name i) FONTSIZE COLOR))

;Exercise 4

;checklist: ShoppingList String PositiveRealNumber -> ShoppingList
;If the new item's name is already on the list, add 1 to the quantity
;If the new item's name is not on the list or it is an "no item" list 
;make a new item with the given name and price and a quantity of 1

(check-expect (checklist SP0 "apple" 1) (make-shopping (make-item "apple" 1 1) "no items"))
(check-expect (checklist SP1 "orange" 4) (make-shopping (make-item "orange" 3 11) "no items"))
(check-expect (checklist SP2 "oil" 3)
              (make-shopping (make-item "apple" 4 5)
                             (make-shopping (make-item "orange" 3 10)
                                            (make-shopping (make-item "oil" 3 1) "no items"))))
(check-expect (checklist SP2 "orange" 3)
              (make-shopping (make-item "apple" 4 5)
                             (make-shopping (make-item "orange" 3 11) "no items")))

(define (checklist sp name price)
  (cond [(string? sp) (make-shopping (make-item name price 1) sp)]
        [(and (shopping? sp) (string=? (item-name (shopping-item sp)) name))
         (make-shopping (make-item name (item-price (shopping-item sp))
                                   (+ 1 (item-quantity (shopping-item sp)))) (shopping-others sp))]
        [else (make-shopping (shopping-item sp) (checklist (shopping-others sp) name price))]))

;Exercise 5

(define-struct date [year month day])
;A Date is a (make-date Nat String Nat)
;Where a year is the numeric year of the event,
;a month is the written month of the event,
;a day is the numeric day of the event,
;and represents a date in a given year

(define DATE1 (make-date 2019 "February" 14))
(define DATE2 (make-date 2019 "December" 25))

;date-temp: Date -> ???
#;(define (date-temp d)
    (... (date-year d) ... (date-month d) ... (date-day d) ...))

(define-struct event [name date])
;An Event is a (make-event String Date)
;Where a name is the name of the event,
;a date is a Date, and represents a
;signifigant event

(define EVENT1 (make-event "Valentine's Day" DATE1))
(define EVENT2 (make-event "Christmas" DATE2))

;event-temp: Event -> ???
#;(define (event-temp e)
    (... (event-name e) ... (date-temp(event-date e))...))

;Exercise 6

(define-struct timeline [event others])
;A Timeline is one of
; - "empty timeline"
; - (make-timeline Event Timeline)
;and represents either an empty timeline or
;a timeline with at least one event in it

(define TL1 (make-timeline EVENT1 ""))
(define TL2 (make-timeline EVENT2 (make-timeline EVENT1 "")))

;timeline-temp: Timeline -> ???
#;(define (timeline-temp t)
    (cond
      [(string? t) ...]
      [(timeline? t)(... (event-temp (timeline-event t))...
                         (timeline-temp (timeline-others t))...)]))

;Exercise 7

; A Digit is a Nat in the range [0,9]

; A Place is a Nat in the range [0,5]

(define-struct zip [a b c d e])
; A Zip is a (make-zip Digit Digit Digit Digit Digit)
; representing a 5-digit zip code

(define ZIP1A (make-zip 0 2 3 6 1))
(define ZIP1B (make-zip 0 3 3 6 1))
(define ZIP2A (make-zip 6 7 5 4 2))
(define ZIP2B (make-zip 3 7 5 4 2))
(define ZIP3A (make-zip 9 9 9 8 9))
(define ZIP3B (make-zip 9 9 9 9 9))
(define ZIP-BOSTON (make-zip 0 2 1 1 5))
(define ZIP-BO-MISTAKE (make-zip 0 2 1 1 9))
(define ZIP-ILLINOIS (make-zip 6 0 2 0 8))
(define ZIP-IL-MISTAKE (make-zip 6 0 0 0 8))

;zip-temp: Zip -> ??? 
#;
(define (zip-temp z)
  (... (zip-a z) ... (zip-b z) ... (zip-c z) ... (zip-d z) ... (zip-e z) ...))

(define-struct addr [street num zip])
; An Address is a (make-addr String Nat ZipCode)
; - where street is the name of the street
; - number is the house number on the street
; - and zip code is the zip code of the address

(define ADDR1 (make-addr "Huntington Ave" 360 ZIP-BOSTON))
(define ADDR2 (make-addr "Clark St" 633 ZIP-ILLINOIS))
(define ADDR2-MISTAKE (make-addr "Clark St" 633 ZIP-IL-MISTAKE))

;addr-temp: Address -> ???
#;(define (addr-temp a)
    (... (addr-street a) ... (addr-num a) ... (addr-zip(zip-temp a)) ...))

(define-struct letter [from to weight])
; A Letter is a (make-letter Address Address PositiveRealNumber)
; - where from is the address the letter is from
; - to is the address to send the letter to
; - and weight is the letter's weight (in ounces)

(define LETTER-BAD (make-letter ADDR1 ADDR2-MISTAKE 5))
(define LETTER-GOOD (make-letter ADDR1 ADDR2 5))

;letter-temp: Letter -> ???
#;(define (letter-temp l)
    (... (letter-from (addr-temp l)) ... (letter-to (addr-temp l)) ... (letter-weight l)))
 
;update-zip-digit: Letter Digit Place -> Letter
;creates a letter with a "to" address containing an updated zipcode

(check-expect (update-zip-digit LETTER-BAD 2 3) LETTER-GOOD)
(check-expect (update-zip-digit LETTER-GOOD 2 3) LETTER-GOOD)

(define (update-zip-digit letter digit place)
  (make-letter (letter-from letter)
               (make-addr
                (addr-street (letter-to letter))
                (addr-num (letter-to letter))
                (zip-change(addr-zip (letter-to letter)) digit place))
               (letter-weight letter)))

;zip-change: Zip Digit Place -> Zip
;changes a specified Digit in a Zip

(check-expect(zip-change ZIP1A 3 2) ZIP1B)
(check-expect(zip-change ZIP2A 3 1) ZIP2B)
(check-expect(zip-change ZIP3A 9 4) ZIP3B)
(check-expect(zip-change ZIP-IL-MISTAKE 2 3) ZIP-ILLINOIS)
(check-expect(zip-change ZIP-BO-MISTAKE 5 5) ZIP-BOSTON)
(check-expect(zip-change ZIP-BOSTON 5 5) ZIP-BOSTON)

(define (zip-change zip digit place)
  (cond
    [(= place 1)(make-zip digit(zip-b zip)(zip-c zip)(zip-d zip)(zip-e zip))]
    [(= place 2)(make-zip (zip-a zip)digit(zip-c zip)(zip-d zip)(zip-e zip))]
    [(= place 3)(make-zip (zip-a zip)(zip-b zip)digit(zip-d zip)(zip-e zip))]
    [(= place 4)(make-zip (zip-a zip)(zip-b zip)(zip-c zip)digit(zip-e zip))]
    [(= place 5)(make-zip (zip-a zip)(zip-b zip)(zip-c zip)(zip-d zip)digit)]))