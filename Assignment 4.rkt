;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Assignment4_Wang) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Exercise 1
;A Posn is a (make-posn NUMBER NUMBER)
;A Posn which x pixels from left, y from top
;manhattan-distance: Posn -> Number 
;Input a posn, returns the distance of the given posn to the origin
(check-expect (manhattan-distance (make-posn 3 4)) 7)
(check-expect (manhattan-distance (make-posn 1 2)) 3)
(check-expect (manhattan-distance (make-posn 0 0)) 0)
(check-expect (manhattan-distance (make-posn -2 3)) 5)
(define (manhattan-distance-temp ap)
  (... (posn-x ap) ... (posn-y ap) ...))
(define (manhattan-distance ap)
  (+ (abs (posn-x ap)) (abs (posn-y ap))))

;Exercise 2
;A Radius is a positive number 
;within-cirle?: Posn1 Posn2 Radius -> Boolean
;Is the first posn located inside of the circle that was centered by the second posn
;with the given radius?
(check-expect (within-circle? (make-posn 1 3) (make-posn 2 3) 5) #true)
(check-expect (within-circle? (make-posn 7 7) (make-posn 0 0) 5) #false)
(check-expect (within-circle? (make-posn -2 -3) (make-posn -1 5) 20) #true)
(define (within-circle? posn1 posn2 r)
  (<= (sqrt (+ (sqr (distance-x posn1 posn2))
               (sqr (distance-y posn1 posn2)))) r))
;distance-x: Posn1 Posn2 -> Posn
;measure the differences of x-coordinates of the two posns
(define (distance-x posn1 posn2)
  (abs (- (posn-x posn1) (posn-x posn2))))
;distance-y: Posn1 Posn2 -> Posn
;Measures the differences of y-coordinates of the two posns
(define (distance-y posn1 posn2)
  (abs (- (posn-y posn1) (posn-y posn2))))

;Exercise 3
(require 2htdp/image)
(require 2htdp/universe)
;Define the constants
(define DOT-RADIUS 20)
(define DOT-IMG (circle DOT-RADIUS "solid" "blue"))
(define WIDTH 300)
(define HEIGHT 300)
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "white"))
(define WIDTH2 (- WIDTH (* 2 DOT-RADIUS)))
(define HEIGHT2 (- HEIGHT (* 2 DOT-RADIUS)))
;A DotState is a (make-posn number number)
;dot-clicker: DotState -> DotState
;move the dot to a random position after the click of a mouse dotton
(define (dot-clicker p)
  (big-bang p
    [to-draw render-image]
    [on-mouse dot-action]))
;render-image: Posn -> Image
;Render the dot image on the background
(check-expect (render-image (make-posn 10 10)) (place-image DOT-IMG 10 10 BACKGROUND))
(check-expect (render-image (make-posn 20 10)) (place-image DOT-IMG 20 10 BACKGROUND))
(define (render-image p)
  (place-image DOT-IMG (posn-x p) (posn-y p) BACKGROUND))
;A MouseEvent deals with a mouse click at (x,y)
;dot-action: DotState Number Number MouseEvent -> DotState
;move the dot to a random position if the mouse is click on the circle
;if the click is not on the circle, the dot remained at its position
(check-expect (posn? (dot-action (make-posn 150 150) 100 20 "button-down")) #true)
(check-expect (posn? (dot-action (make-posn 200 200) 200 200 "button-down")) #true)
(check-expect (dot-action (make-posn 150 150) 0 0 "button-up") (make-posn 150 150))
(check-expect (dot-action (make-posn 150 150) 0 0 "button-down") (make-posn 150 150))
(define (dot-action-temp p mx my me)
  (... (string=? me ...) ... (make-posn mx my) ...))
(define (dot-action p mx my me)
  (cond [(and (string=? me "button-down") (within-circle? (make-posn mx my) p DOT-RADIUS))
         (make-posn (random WIDTH2) (random HEIGHT2))]
        [else p]))
(dot-clicker (make-posn 150 150)) 
                                    
;Exercise 4
;Posn is a structure combines two components of any kind of data values into one single value
;while make-posn is an operating function that consumes two data values and makes a posn

;Exercise 5
;A DotState is a (make-posn NaturalNumber NaturalNumber)
;dot-mover: DotState -> DotState
(define (dot-mover p)
  (big-bang p
    [to-draw render-image]
    [on-key dot-action2]))
;dot-action: DotState KeyEvent -> DotState
;Move the dot right 5 pixels when right arrow is hitted
;Move the dot left 5 pixels when left arrow is hitted
;Move the dot down 5 pixels when down arrow is hitted
;Move the dot up 5 pixels when up arrow is hitted
;Returns to the opposite side of the background when the dot hit the edge 
(check-expect (dot-action2 (make-posn 150 150) "right") (make-posn 155 150))
(check-expect (dot-action2 (make-posn 150 150) "left") (make-posn 145 150))
(check-expect (dot-action2 (make-posn 150 150) "up") (make-posn 150 145))
(check-expect (dot-action2 (make-posn 150 150) "down") (make-posn 150 155))
(check-expect (dot-action2 (make-posn 320 150) "right") (make-posn (- DOT-RADIUS) 150))
(check-expect (dot-action2 (make-posn -20 150) "left") (make-posn (+ WIDTH DOT-RADIUS) 150))
(check-expect (dot-action2 (make-posn 150 -20) "up") (make-posn 150 (+ DOT-RADIUS HEIGHT)))
(check-expect (dot-action2 (make-posn 150 320) "down") (make-posn 150 (- DOT-RADIUS)))
(check-expect (dot-action2 (make-posn 150 150) "space") (make-posn 150 150))
(define (dot-action2 p k)
  (cond [(and (right? p) (string=? k "right"))
         (make-posn (- DOT-RADIUS) (posn-y p))]
        [(and (left? p) (string=? k "left"))
         (make-posn (+ WIDTH DOT-RADIUS) (posn-y p))]
        [(and (up? p) (string=? k "up"))
         (make-posn (posn-x p) (+ DOT-RADIUS HEIGHT))]
        [(and (down? p) (string=? k "down"))
         (make-posn (posn-x p) (- DOT-RADIUS))]
        [else (cond [(string=? k "right") (make-posn (+ 5 (posn-x p)) (posn-y p))]
                    [(string=? k "left") (make-posn (- (posn-x p) 5) (posn-y p))]
                    [(string=? k "up") (make-posn (posn-x p) (- (posn-y p) 5))]
                    [(string=? k "down") (make-posn (posn-x p) (+ (posn-y p) 5))]
                    [else p])]))
;right?: Posn -> Boolean
;check whether the dot hit the right edge
(check-expect (right? (make-posn 100 100)) #false)
(check-expect (right? (make-posn 320 100)) #true)
(define (right? p)
  (>= (posn-x p) (+ WIDTH DOT-RADIUS)))
;left?: Posn -> Boolean
;check whether the dot hit the left edge
(check-expect (left? (make-posn 100 100)) #false)
(check-expect (left? (make-posn -20 100)) #true)
(define (left? p)
  (<= (posn-x p) (- 0 DOT-RADIUS)))
;down?: Posn -> Boolean
;check whether the dot hit the bottom edge
(check-expect (down? (make-posn 100 100)) #false)
(check-expect (down? (make-posn 100 320)) #true)
(define (down? p)
  (>= (posn-y p) (+ HEIGHT DOT-RADIUS)))
;up?: Posn -> Boolean
;check whether the dot hit the upper edge
(check-expect (up? (make-posn 100 100)) #false)
(check-expect (up? (make-posn 100 -20)) #true)
(define (up? p)
  (<= (posn-y p) (- 0 DOT-RADIUS)))

(dot-mover (make-posn 150 150))
                                   

  

              
