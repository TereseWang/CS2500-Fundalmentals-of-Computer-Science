;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW-15-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Exercise 1
(define-struct branch [val left right])
; A [Tree X] is one of:
; - "leaf"
; - (make-branch X [Tree X] [Tree X])
; and represents a tree that contains data of type X
(define TREE-0 "leaf")
(define TREE-1 (make-branch 5 TREE-0 TREE-0))
(define TREE-2 (make-branch 8 TREE-0 TREE-0))
(define TREE-3 (make-branch 2 (make-branch 1 "leaf" "leaf") TREE-0))
(define TREE-4 (make-branch 7 TREE-1 TREE-2))
(define TREE-5 (make-branch 4 TREE-3 TREE-4))
(define TREE-6 (make-branch "cat"
                            (make-branch "ba" "leaf" "leaf")
                            (make-branch "dbt" "leaf" "leaf")))
(define TREE-7 (make-branch "duck"
                            (make-branch "doge" "leaf" "leaf")
                            (make-branch "shucks" "leaf" "leaf")))
(define TREE-8 (make-branch "dog" TREE-6 TREE-7))

#;
(define (branch-temp b)
  (cond [(string? b) ...]
        [(branch? b) ... (branch-val b)
                     (branch-temp (branch-left b))
                     (branch-temp (branch-right b))]))
; A [Comparator X] is a [X X -> Number] functiDeon such that 
; - (f a b) < 0 if a < b
; - (f a b) = 0 if a = b
; - (f a b) > 0 if a > b

(define-struct bst [comparator tree])
; A [BST X] is a (make-bst [Comparator X] [Tree X])
; and represents a binary search tree that contains data of type X with no repeats
; For some (make-bst f (make-branch x y z))
; - x is greater than every element in the tree y according to the comparator f
; - x is less than every element in the tree z according to the comparator f
; - y and z are are also ordered according to the comparator
; Note: (make-branch x "leaf" "leaf") is ordered by any comparator
(define BST-1 (make-bst (λ (x y) (- x y)) TREE-4))
(define BST-2 (make-bst (λ (x y) (- x y)) TREE-5))
(define BST-3 (make-bst (λ (x y)
                          (- (foldr + 0 (map char->integer (string->list x)))
                             (foldr + 0 (map char->integer (string->list y))))) TREE-8))
#;
(define (bst-temp bst)
  ... (bst-comparator bst)
  ... (branch-temp (bst-tree bst)))
                
;Exercise 2:
;in-bst?: [BST X] X -> Boolean
;Does the given [BST X] contain the gievm value
(check-expect (in-bst? BST-1 4) #f)
(check-expect (in-bst? BST-2 4) #t)
(check-expect (in-bst? BST-3 "duck") #t)
(check-expect (in-bst? BST-3 "horse") #f)
(define (in-bst? bst x)
  (cond
    [(string? (bst-tree bst)) #f]
    [(< ((bst-comparator bst) x (branch-val (bst-tree bst))) 0)
     (in-bst? (make-bst (bst-comparator bst) (branch-left (bst-tree bst))) x)]
    [(> ((bst-comparator bst) x (branch-val (bst-tree bst))) 0)
     (in-bst? (make-bst (bst-comparator bst) (branch-right (bst-tree bst))) x)]
    [(= ((bst-comparator bst) x (branch-val (bst-tree bst))) 0) #t]))

;Exercise 3:
;swap-bst: [BST X] -> [BST X]
;Produce the opposite of the given [BST X]
;Everything on the left of the tree should be switched to the right, vice versa.
(check-expect (bst-tree (swap-bst BST-1))
              (make-branch 7 (make-branch 8 "leaf" "leaf")
                           (make-branch 5 "leaf" "leaf")))
(check-expect (bst-tree (swap-bst BST-2))
              (make-branch 4
                           (make-branch 7
                                        (make-branch 8 "leaf" "leaf")
                                        (make-branch 5 "leaf" "leaf"))
                           (make-branch 2
                                        "leaf"
                                        (make-branch 1 "leaf" "leaf"))))
(check-expect (in-bst? (swap-bst BST-1) 5) #t)
(check-expect (in-bst? (swap-bst BST-2) 9) #f)
(check-expect (in-bst? (swap-bst BST-2) 7) #t)                                           
(define (swap-bst bst)
  (make-bst (λ (x y) ((bst-comparator bst) y x)) (swap-branch (bst-tree bst))))
;swap-branch: [Tree X] -> [Tree X]
;Produce the opposite of the given [Tree X]
;Everything on the left of the tree should be switched to the right, vice versa.
(check-expect (swap-branch TREE-4)
              (make-branch 7
                           (make-branch 8 "leaf" "leaf")
                           (make-branch 5 "leaf" "leaf")))
(check-expect (swap-branch TREE-5)
              (make-branch 4
                           (make-branch 7
                                        (make-branch 8 "leaf" "leaf")
                                        (make-branch 5 "leaf" "leaf"))
                           (make-branch 2
                                        "leaf"
                                        (make-branch 1 "leaf" "leaf"))))
(define (swap-branch br)
  (cond [(string? br) "leaf"]
        [else (make-branch (branch-val br)
                           (swap-branch (branch-right br))
                           (swap-branch (branch-left br)))]))

;Exercise 4:
;place-in-tree: [BST X] X -> [BST X]
;Add the value x to the correct location of the given [BST X]
(check-expect (bst-tree (place-in-bst BST-1 10))
              (make-branch 7 (make-branch 5 "leaf" "leaf")
                           (make-branch 8 "leaf" (make-branch 10 "leaf" "leaf"))))
(check-expect (bst-tree (place-in-bst BST-1 8))
              (make-branch 7 (make-branch 5 "leaf" "leaf") (make-branch 8 "leaf" "leaf")))
(check-expect (bst-tree (place-in-bst BST-1 3))
              (make-branch 7 (make-branch 5 (make-branch 3 "leaf" "leaf") "leaf")
                           (make-branch 8 "leaf" "leaf")))
(check-expect (in-bst? (place-in-bst BST-1 10) 10) #t)
(define (place-in-bst bst x)
  (local [(define (place-in-tree b)
            (cond [(string? b)
                   (make-branch x "leaf" "leaf")]
                  [(branch? b)
                   (cond
                     [(< ((bst-comparator bst) x (branch-val b)) 0)
                      (make-branch (branch-val b) (place-in-tree (branch-left b)) (branch-right b))]
                     [(> ((bst-comparator bst) x (branch-val b)) 0)
                      (make-branch (branch-val b) (branch-left b) (place-in-tree (branch-right b)))]
                     [else b])]))]
    (make-bst (bst-comparator bst) (place-in-tree (bst-tree bst)))))

;Exercise 5
; A Choice is a (make-choice String Section)
(define-struct choice [text result])
; and represents the blurb shown to the reader and the resulting section if that is the
; choice they make
;choice-temp: Choice -> ???
#;
(define (choice-temp c)
  ... (choice-text c)
  ... (section-temp (choice-result c)))

; An [NEList-of X] (Non-Empty List) is one of:
; - (cons X '())
; - (cons X [NEList-of X])
;nelist-temp: [NEList-of X] -> ???
#;
(define (nelist-temp nlist)
  (cond [(empty? (rest nlist)) ...]
        [else (... (first nlist)
                   ... (nelist-temp (rest nlist)))]))

; A Chapter is a (make-chapter String [NEList-of Choice])
(define-struct chapter [text choices])
; and represents a chapter in a choose-your-own adventure book with a body
; and a list of choices at the end
;chapter-temp: Chapter -> ???
#;
(define (chapter-temp ch)
  ... (chapter-text ch)
  ... (nelist-temp (chapter-choices ch)))

; An Ending is a (make-ending String Boolean)
(define-struct ending [text good?])
; and represents an ending to the story with some text and whether it was a happy ever after
;ending-temp: Ending -> ???
#;
(define (ending-temp end)
  ... (ending-text end)
  ... (ending-good? end))

; A Section is one of:
; - Ending
; - Chapter
; and represents a section in a choose your own adventure book
;section-temp:  Section -> ???
#;
(define (section-temp sec)
  (cond [(ending? sec) ... (ending-temp sec)]
        [(chapter? sec) ... (chapter-temp sec)]))

(define MY-STORY
  (make-chapter
   "You are alone in a room. There is a door before you."
   (list
    (make-choice
     "Stay in the room."
     (make-ending "You stay in the room. Nothing happens. Nothing ever will again." #f))
    (make-choice
     "Open the door."
     (make-chapter
      "You open the door. On the other side lays an eternity of nothingness."
      (list
       (make-choice
        "Scream."
        (make-ending
         "You scream into the void. There is no response. There never will be." #f))
       (make-choice
        "Accept your fate."
        (make-ending
         "You accept the simple beauty of the void. You achieve nirvana." #t))))))))
(define NELIST1
  (list
   (make-choice
    "Scream."
    (make-ending
     "You scream into the void. There is no response. There never will be." #f))
   (make-choice
    "Accept your fate."
    (make-ending
     "You accept the simple beauty of the void. You achieve nirvana." #t))))
(define NELIST2
  (list
   (make-choice
    "Scream."
    (make-ending
     "You scream into the void. There is no response. There never will be." #f))
   (make-choice
    "Accept your fate."
    (make-ending
     "You accept the simple beauty of the void. You achieve nirvana." #t))
   (make-choice
    "Accept your fate."
    (make-ending
     "You accept the simple beauty of the void. You achieve nirvana." #t))
   (make-choice
    "Accept your fate."
    (make-ending
     "You accept the simple beauty of the void. You achieve nirvana." #t))))
(define MY-STORY1
  (make-chapter
   "You open the door. On the other side lays an eternity of nothingness." NELIST2))
(define MY-STORY2
  (make-chapter
   "You are alone in a room. There is a door before you."
   (list
    (make-choice
     "Stay in the room."
     (make-ending "You stay in the room. Nothing happens. Nothing ever will again." #f))
    (make-choice
     "Open the door."
     MY-STORY1))))

;Exercise 6
;count-ending: Section -> Nat
;count the number of Endings in a Section
(check-expect (count-ending MY-STORY) 3)
(check-expect (count-ending MY-STORY1) 4)
(define (count-ending sec)
  (cond [(ending? sec) 1]
        [(chapter? sec) (count-list (chapter-choices sec))]))

;count-list: [NEList-of Choice] -> Nat
;count the number of Endings in a [NEList-of Choice]
(check-expect (count-list NELIST1) 2)
(check-expect (count-list NELIST2) 4)
(define (count-list nlist)
  (cond [(empty? (rest nlist)) (count-choice (first nlist))]
        [else (+ (count-choice (first nlist))
                 (count-list (rest nlist)))]))

;count-choice: Choice -> Nat 
;count the number of Endings in a choice
(check-expect (count-choice
               (make-choice
                "Scream."
                (make-ending
                 "You scream into the void. There is no response. There never will be." #f))) 1)
(check-expect (count-choice
               (make-choice
                "Accept your fate."
                (make-ending
                 "You accept the simple beauty of the void. You achieve nirvana." #t))) 1)
(define (count-choice c)
  (count-ending (choice-result c)))

;Exercise 7
(require 2htdp/image)
(require 2htdp/universe)
(define HEIGHT 600)
(define WIDTH 1200)
(define BG (rectangle WIDTH HEIGHT "solid" "white"))
(define FONT 24)

(define-struct adv-game [section current])
;An Game is a a (make-adv-game Section Nat)
;Where current is the player's current choice selection
(define ADV-GAME1 (make-adv-game MY-STORY 0))
(define ADV-GAME2 (make-adv-game MY-STORY2 1))
(define ADV-GAME3 (make-adv-game (make-ending "GAME OVER" #f) 0))
;adv-game-temp: Game -> ???
#;
(define (adv-game-temp g)
  (... (section-temp (adv-game-section g)) ... (adv-game-current ag)))

;adventure-game: Section -> Game
;Starts the game and allows player to move through the game
(define (adventure-game section)
  (big-bang (make-adv-game section 0)
    [to-draw render-section]
    [on-key choice-select]
    [stop-when adv-game-ending? render-section]))

;render-section: Game -> Image
;Render the Section of the game 
(check-expect (render-section ADV-GAME1) (render-chapter MY-STORY 0))
(check-expect (render-section ADV-GAME2) (render-chapter MY-STORY2 1))
(check-expect (render-section ADV-GAME3) (render-ending (make-ending "GAME OVER" #f)))
(define (render-section ag)
  (cond [(ending? (adv-game-section ag))
         (render-ending (adv-game-section ag))]
        [(chapter? (adv-game-section ag))
         (render-chapter (adv-game-section ag) (adv-game-current ag))]))

;render-ending: Ending -> Image
;render the Ending result of the game onto the background 
(check-expect (render-ending (make-ending "GAME OVER" #f))
              (place-image (text "GAME OVER" FONT "red")
                           (/ WIDTH 2) (/ HEIGHT 2) BG))
(check-expect (render-ending (make-ending "GAME OVER!" #t))
              (place-image (text "GAME OVER!" FONT "blue")
                           (/ WIDTH 2) (/ HEIGHT 2) BG))
(define (render-ending end)
  (place-image
   (if (ending-good? end)
       (text (ending-text end) FONT "blue")
       (text (ending-text end) FONT "red"))
   (/ WIDTH 2) (/ HEIGHT 2) BG))

;render-chapter: Chapter Nat -> Image
;render the chapter of the game on to the background 
(check-expect (render-chapter MY-STORY 0)
              (place-image
               (above
                (text "You are alone in a room. There is a door before you." FONT "black")
                (above
                 (text "Stay in the room." FONT "orange")
                 (text "Open the door." FONT "black")))
               (/ WIDTH 2) (/ HEIGHT 2) BG))
(check-expect (render-chapter MY-STORY2 1)
              (place-image
               (above
                (text "You are alone in a room. There is a door before you." FONT "black")
                (above
                 (text "Stay in the room." FONT "black")
                 (text "Open the door." FONT "orange")))
               (/ WIDTH 2) (/ HEIGHT 2) BG))
(define (render-chapter chap curr)
  (place-image
   (above (text (chapter-text chap) FONT "black")
          (render-nelochoices (chapter-choices chap) curr))
   (/ WIDTH 2) (/ HEIGHT 2) BG))

;render-nelochoices: [NEList-of Choice] Nat -> Image
;Render the list of choices onto the background 
(check-expect (render-nelochoices (chapter-choices MY-STORY) 0)
              (above
               (text "Stay in the room." FONT "orange")
               (text "Open the door." FONT "black")))
(check-expect (render-nelochoices (chapter-choices MY-STORY2) 1)
              (above
               (text "Stay in the room." FONT "black")
               (text "Open the door." FONT "orange")))
(define (render-nelochoices nelochoices curr)
  (local [(define (render-choice c)
            (if (= curr 0)
                (text (choice-text c) FONT "orange")
                (text (choice-text c) FONT "black")))]
    (cond [(empty? (rest nelochoices)) (render-choice (first nelochoices))]
          [else (above (render-choice (first nelochoices))
                       (render-nelochoices (rest nelochoices) (sub1 curr)))])))

;choice-select: Game KeyEvent -> Game
;goes to the previous or next choice after pressing "up" or "down"
;takes the player to the next state of the game after pressing enter 
(check-expect (choice-select ADV-GAME1 "up") (make-adv-game MY-STORY (modulo (sub1 0) 2)))
(check-expect (choice-select ADV-GAME1 "down") (make-adv-game MY-STORY (modulo 1 2)))
(check-expect (choice-select ADV-GAME2 "\r") (select-choice ADV-GAME2))
(define (choice-select ag key)
  (cond
    [(string=? key "up")
     (make-adv-game
      (adv-game-section ag)
      (modulo (sub1 (adv-game-current ag)) (length (chapter-choices (adv-game-section ag)))))]
    [(string=? key "down")
     (make-adv-game
      (adv-game-section ag)
      (modulo (add1 (adv-game-current ag)) (length (chapter-choices (adv-game-section ag)))))]
    [(string=? key "\r") (select-choice ag)]))

 
;select-chocie: Game -> Game
;Takes the player to the results of their choices
;Either to an ending or a new choice 
(check-expect (select-choice ADV-GAME1)
              (make-adv-game
               (make-ending "You stay in the room. Nothing happens. Nothing ever will again." #f) 0))
(check-expect (select-choice ADV-GAME2)
              (make-adv-game MY-STORY1 0))
(define (select-choice ag)
  (make-adv-game
   (choice-result (list-ref (chapter-choices (adv-game-section ag)) (adv-game-current ag))) 0))

;adv-game-ending?: Game -> Boolean
;Does the game end?
(check-expect (adv-game-ending? ADV-GAME1) #f)
(check-expect (adv-game-ending? (make-adv-game (make-ending "GAME OVER" #f) 0)) #t)
(define (adv-game-ending? ag)
  (ending? (adv-game-section ag)))

(adventure-game MY-STORY)
