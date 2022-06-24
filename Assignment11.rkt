;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Assignment-11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Exerise 1:
;A ListofBooleans is one:
; - '()
; - (cons Boolean ListofBooleans)
(define LOB-0 '())
(define LOB-1 (cons #t LOB-0))
(define LOB-2 (cons #t LOB-1))
(define LOB-3 (cons #f LOB-2))
(define LOB-4 (cons #f LOB-1))
(define LOB-5 (cons #f LOB-4))

;lob-temp: ListofBooleans -> ???
#;
(define (lob-temp lob)
  (cond
    [(empty? lob) ...]
    [(cons? lob) (... (first lob)
                      ... (lob-temp (rest lob)))]))

;all-true: ListofBooleans -> Boolean
;Are all of the elements in the list true?
(check-expect (all-true LOB-0) #t)
(check-expect (all-true LOB-2) #t)
(check-expect (all-true LOB-3) #f)
(define (all-true lob)
  (cond
    [(empty? lob) #t]
    [(cons? lob) (and (first lob) (all-true (rest lob)))]))

;one-true: ListofBooleans -> Boolean
;Is there at least one true in the list?
(check-expect (one-true LOB-0) #f)
(check-expect (one-true LOB-1) #t)
(check-expect (one-true LOB-5) #t)
(define (one-true lob)
  (cond
    [(empty? lob) #f]
    [(cons? lob) (or (first lob) (one-true (rest lob)))]))

;Exercise 2
;The function names should have a ?,
;since they both return booleans

;Exercise 3
(require 2htdp/Image)

(define SQUARE-1 (square 20 "solid" "black"))
(define SQUARE-2 (square 50 "solid" "blue"))
(define RECT-1 (rectangle 20 50 "solid" "grey"))


;A ListofImages is one:
; - '()
; - (cons Image ListofImages)
(define LOI-0 '())
(define LOI-1 (cons SQUARE-1 LOI-0))
(define LOI-2 (cons SQUARE-1 LOI-1))
(define LOI-3 (cons SQUARE-2 LOI-2))
(define LOI-4 (cons RECT-1 LOI-2))

;loi-temp: ListofImages -> ???
#;
(define (loi-temp loi)
  (cond
    [(empty? loi) ...]
    [(cons? loi) (... (first loi)
                      ... (loi-temp (rest loi)))]))

;ImageOrFalse is one of:
; - Image
; - #false

;ill-sized?: ListofImages PositiveNumber -> ImageOrFalse
;What is the first image to not be a square of the specified lengths?
(check-expect (ill-sized? LOI-0 10) #f)
(check-expect (ill-sized? LOI-2 20) #f)
(check-expect (ill-sized? LOI-3 20) SQUARE-2)
(check-expect (ill-sized? LOI-4 20) RECT-1)
(define (ill-sized? loi num)
  (cond
    [(empty? loi) #f]
    [(cons? loi) (if (matching-square? (first loi) num)
                     (ill-sized? (rest loi) num)
                     (first loi))]))

;matching-square?: Image PostiveNumber -> Boolean
;Is the image a square with the specified side lenghths?
(check-expect (matching-square? SQUARE-1 20) #t)
(check-expect (matching-square? SQUARE-2 20) #f)
(check-expect (matching-square? RECT-1 50) #f)
(define (matching-square? i num)
  (and (= (image-width i) num) (= (image-height i) num)))

;Exercise 4:
;sum and how-many will still work for the new data definition
;because it will still return to the base case of it being empty.
;However, it would be incorrect in the sense that it isn't following
;the new template that the data definition would require

;Exercise 5:

; A Nat is one of:
; - 0
; - (add1 Nat)
;nat-temp: Nat -> ???
#;
(define (nat-temp n)
  (cond
    [(zero? n) ...]
    [(positive? n) (... (nat-temp (sub1 n)) ...)]))

;image-placer: Image Nat -> Image
;places the image beside itself the specified number of times
(check-expect (image-placer SQUARE-1 0) empty-image)
(check-expect (image-placer SQUARE-2 2) (beside SQUARE-2 SQUARE-2))
(check-expect (image-placer RECT-1 3) (beside RECT-1 RECT-1 RECT-1))
(define (image-placer i num)
  (cond
    [(zero? num) empty-image]
    [(positive? num) (beside i (image-placer i (sub1 num)))]))

;Exercise 6:
(define-struct story [height rooms color])
; A Story is a (make-story PositiveInteger PositiveInteger Color)
; - where height is the height of the story (in pixels)
; - rooms is the number of rooms on this floor of the building
; - and color is the color of this floor of the building
(define STORY-1 (make-story 10 5 "red"))
(define STORY-2 (make-story 20 3 "blue"))
(define STORY-3 (make-story 15 7 "yellow"))
;story-temp: Story -> ???
#;
(define (story-temp st)
  (... (story-height st)... (story-rooms st)... (story-color st)))

; A ListOfStories is one of:
; - '()
; - (cons Story ListOfStories)
(define LOS-0 '())
(define LOS-1 (cons STORY-1 LOS-0))
(define LOS-2 (cons STORY-2 LOS-1))
(define LOS-3 (cons STORY-3 LOS-2))
;los-temp: ListOfStories -> ???
#;
(define (los-temp los)
  (cond
    [(empty? los) ...]
    [(cons? los) (... (story-temp (first los))... (los-temp (rest los)))]))

(define-struct building [name zip floors])
; A Building is a (make-building String Nat ListOfStories)
; - where name is the building's name
; - zip is the building's zip code
; - and floors is a list of all the floors in the building
(define BUILDING-0 (make-building "Vacant Lot" 13412 LOS-0))
(define BUILDING-1 (make-building "Dodge" 00101 LOS-1))
(define BUILDING-2 (make-building "Loftman" 22046 LOS-2))
(define BUILDING-3 (make-building "Ell Hall" 10010 LOS-3))
;building-temp: Building -> ???
#;
(define (building-temp b)
  (... (building-name b)... (building-zip b)... (los-temp (building-floors b))))

;Exercise 7:

;most-rooms-building: Building -> PositiveInteger
;returns the number of rooms in the story with the most rooms in the building
(check-expect (most-rooms-building BUILDING-0) 0)
(check-expect (most-rooms-building BUILDING-1) 5)
(check-expect (most-rooms-building BUILDING-2) 5)
(check-expect (most-rooms-building BUILDING-3) 7)
(define (most-rooms-building b)
  (if (empty? (building-floors b)) 0 (most-rooms-stories (building-floors b))))

;most-rooms-stories: ListofStories -> PositiveInteger
;returns the number of rooms in the story with the most rooms in the list of stories
(check-expect (most-rooms-stories LOS-0) 0)
(check-expect (most-rooms-stories LOS-1) 5)
(check-expect (most-rooms-stories LOS-2) 5)
(check-expect (most-rooms-stories LOS-3) 7)
(define (most-rooms-stories los)
  (cond
    [(empty? los) 0]
    [(cons? los) (max (story-rooms (first los))
                      (most-rooms-stories (rest los)))]))

;Exercise 8:
(define WIDTH 100)

;draw-building: Building -> Image
;returns an image representing a building
(check-expect (draw-building BUILDING-0) empty-image)
(check-expect (draw-building BUILDING-1) (draw-los LOS-1))
(check-expect (draw-building BUILDING-3) (draw-los LOS-3))
(define (draw-building b)
  (draw-los (building-floors b)))

;draw-los: ListofStories -> Image
;returns an image of all of the stories stacked on top each other
(check-expect (draw-los LOS-0) empty-image)
(check-expect (draw-los LOS-1) (story-image STORY-1))
(check-expect (draw-los LOS-3) (above (story-image STORY-3)
                                      (story-image STORY-2)
                                      (story-image STORY-1)))
(define (draw-los los)
  (cond
    [(empty? los) empty-image]
    [(cons? los) (above (story-image (first los))
                        (draw-los (rest los)))]))

;story-image: Story -> Image
;returns an image representing an entire story with rooms
(check-expect (story-image (make-story 10 0 "red")) empty-image)
(check-expect (story-image STORY-2) (beside (room-image STORY-2)
                                            (room-image STORY-2)
                                            (room-image STORY-2)))
(check-expect (story-image STORY-3) (beside (room-image STORY-3)
                                            (room-image STORY-3)
                                            (room-image STORY-3)
                                            (room-image STORY-3)
                                            (room-image STORY-3)
                                            (room-image STORY-3)
                                            (room-image STORY-3)))
(define (story-image st)
  (cond
    [(zero? (story-rooms st)) empty-image]
    [else (image-placer (room-image st) (story-rooms st))]))
     

;room-image: Story -> Image
;Returns a rectangle representing the room in a story
(check-expect (room-image STORY-1) (frame (rectangle (/ WIDTH 5) 10 "solid" "red")))
(check-expect (room-image STORY-3) (frame (rectangle (/ WIDTH 7) 15 "solid" "yellow")))
(define (room-image st)
  (frame (rectangle
          (/ WIDTH (story-rooms st))
          (story-height st)
          "solid"
          (story-color st))))