;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Assignment7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define FIXING-STYLE-ISSUES #true)

;Exercise 1
;This problem is a recursive problem
;which means the interpretation part should looks something like this
;A MonkeyChain is one of
;- "empty"
;- (make-monkey String String MonkeyChain)
;Besides, it does not include the case which there are no other monkeys

;Exercise 2
;sd-temp: StudentDatabase -> ???
#;(define (sd-temp db)
    (cond [(string? db) ... ]
          [(database? db)
           (... (entry-temp (database-entry db)) ...
                (sd-temp (database-others db)) ...)]))

;entry-temp: StudentEntry -> ???
#;(define (entry-temp db)
    (... (entry-name db) ...
         (entry-nuin db) ...
         (entry-gradyear db) ...))

;Exercise 3
;The first one is better, since it may be easier to work with
;which when we need to deal with make-database functions, we do not need
;to wrtite everything out like (make-database name nuid gradyear others)
;rather we can just define entries and put it in the first database structure
;this will makes the function more organized and easier to read

;Exercise 4
;(define-struct listing [address information others])
;A Listing is one of
;- "Zero listing"
;- (make-listing Address Information Listing)
;Where an others is other real estate listings
;and represents either no listings or at least one listing
;with information about the house
;(define LIST0 "empty")
;(define LIST1 (make-listing AD1 INF1 LIST0))
;(define LIST2 (make-listing AD2 INF2 LIST1))
;listing-temp: Listing -> ???
#;(define (listing-temp ls)
    (cond [(string? ls) ...]
          [(listing? ls)
           (... (listing-address ls) ...
                (listing-information ls) ...
                (listing-temp (listing-others ls)) ...)]))

;(define-struct address [house street zip])
;An Address is a (make-address Nat String Nat)
;Interpretation: represents the address of the house 
;where a house is the house number
;a street is the street name of the house
;A zip is the zip code of the hosue
;(define AD1 (make-address 75 "School Street" 02113))
;(define AD2 (make-address 34 "Center Road" 03455))
;address-temp: Address -> ???
#;(define (address-temp a)
    (... (address-house a) ... (address-street a) ... (address-zip a) ...))

;(define-struct information [price beds baths])
;An Information is a (make-information PosNum Nat Nat) 
;Where a price is the price for the house
;A beds is the number of bedrooms in the house
;A baths is the number of bathrooms in the house
;(define INF1 (make-information 300 2 1))
;(define INF2 (make-information 100 3 2))
;information-temp: Information -> ???
#;(define (information-temp a)
    (... (information-price a) ... (information-beds a) ... (information-baths a) ...))

;Exercise 5
;The phrase "this year" is too vague, which it did not specifiy which year
;the students are graduating.
;The year should be specific like 2019
;The signature should not only takes in a StudentDatabase
;It should also takes in the specifc year, like:
;num-students: StudentDatabase Nat -> Nat
;Takes in a StudentDatabase as well as the given year
;and produces a Nat representing the number
;of students with the same gradyear of the given year 

;Exercise 6
;It is unnecessary to question whether or not the function is a database in the second condition,
;since we had mentioned in the signature that the input will be a StudentDatabase
;Besides it does not make sense to use if within a code when you can just add a
;second condition.It is less confusing and works the same way 
;(define ENTRY1 (make-entry "husky" 0023 2018))
;(define ENTRY2 (make-entry "sdhei" 0045 2019))
;(define ENTRY3 (make-entry "sffrg" 0012 2019))
;(define DB0 "empty database")
;(define DB1 (make-database ENTRY1 DB0))
;(define DB2 (make-database ENTRY2 DB1))
;(define DB3 (make-database ENTRY3 DB2))
;(check-expect (num-students DB0) 0)
;(check-expect (num-students DB1) 0)
;(check-expect (num-students DB2) 1)
;(check-expect (num-students DB3) 2)

;Exercise 7
;First he had included title in his define-struct slideshow function
;but it was not used anywhere in the code
;This could be fixed by either including title in the code, or removing it
;from the structure definition
;constants and templates for SlideShow
;(require 2htdp/image)
;(define SSHOW0 (make-slideshow "slideshow" "empty"))
;(define IMG0 (circle 50 "solid" "red"))
;(define IMG1 (circle 50 "solid" "blue"))
;(define SSHOW1 (make-slideshow "slideshow" (make-sset IMG0 "empty")))
;(define SSHOW2 (make-slideshow "slideshow" (make-sset IMG0 (make-sset IMG1 "empty"))))

;slideshow-temp: SlideShow -> ???
#;(define (slideshow-temp sd)
    (... (slideshow-title sd) ... (slide-show-slides sd) ...))
;constants and templates for SlideSet
;(define SSET0 "empty")
;(define SSET1 (make-sset IMG SSET0))
;slideset-temp: SlideSet -> ???
#;(define (slideset-temp ss)
    (cond [(string? ss) ...]
          [else (slideset-temp (sset-other-slides ss)) ...]))
;Signature and Purpose Statement
;display-all-slides: SlideShow -> Image
;shows either no slides or at least one slides next to others
;Examples
;(check-expect (display-all-slides SSHOW0) empty-image)
;(check-expect (display-all-slides SSHOW1) IMG0)
;(check-expect (display-all-slides SSHOW2) (beside IMG0 IMG1))


  
             


