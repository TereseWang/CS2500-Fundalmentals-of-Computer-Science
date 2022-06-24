;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 16|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Exercise 1
(define-struct stack [scoops])
(define-struct topping [type ic])
(define-struct swirl [type1 type2])
; A Swirl is a (make-swirl IceCream IceCream)
; - where type1 represents the first type of ice cream to swirl
; - and type2 represents the second type of ice cream to swirl
;swirl-temp: Swirl -> ???
#;
(define (swirl-temp sw)
  (... (ice-cream-temp (swirl-type1 sw))
       ... (ice-cream-temp (swirl-type2 sw))))
(define SWIRL1 (make-swirl "vanilla"  "strawberry"))
(define SWIRL2 (make-swirl "banana" "chocolate"))
(define SWIRL3 (make-swirl "lemon" "strawberry"))
(define SWIRL4 (make-swirl "vanilla" "banana"))
(define SWIRL5 (make-swirl SWIRL1 SWIRL2))
(define SWIRL6 (make-swirl SWIRL3 SWIRL4))
(define SWIRL7 (make-swirl SWIRL2 SWIRL4))
(define SWIRL8 (make-swirl SWIRL1 SWIRL3))
(define SWIRL9 (make-swirl (make-stack (list "vanilla" "strawberry" "vanilla"))
                           (make-topping "syrup" "vanilla")))
; An IceCream is one of:
; - String (representing a single ice cream flavor)
; - Swirl (representing two ice creams swirled together)
; - Sundae (representing an ice cream sundae)
;ice-cream-temp: IceCream -> ???
#;
(define (ice-cream-temp icecream)
  (cond [(string? icecream) ...]
        [(swirl? icecream) (swirl-temp icecream)]
        [else (sundae-temp icecream)]))
(define IC1 "vanilla")
(define IC2 "strawberry")
(define IC3 "banana")
(define IC4 "chocolate")
(define IC5 "lemon")
(define IC6 SWIRL1)
(define IC7 SWIRL5)
(define IC8 (make-stack (list IC1 IC2 IC3 IC5 IC6 SWIRL1 SWIRL2 SWIRL3)))
(define IC9 (make-topping "syrup" IC8))
(define IC10 (make-stack (list IC1 IC2 IC3 SWIRL9
                               (make-stack (list IC6 IC1))
                               (make-topping "syrup" IC1))))
; A Sundae is one of:
; - (make-stack [List-of IceCream])
;   representing a stack of ice cream scoops
; - (make-topping String IceCream)
;   representing a topping on top of some ice cream
;sundae-temp: Sundae -> ???
#;
(define (sundae-temp sundae)
  (cond [(stack? sundae) (stack-temp sundae)]
        [(topping? sundae) (topping-temp sundae)]))
(define SUND-0 (make-stack (list IC1)))
(define SUND-1 (make-stack (list IC1 IC2)))
(define SUND-2 (make-stack (list IC5 IC6 SWIRL1 SWIRL2 SWIRL3)))
(define SUND-3 (make-stack (list IC4 IC7 IC3)))
(define SUND-4 (make-topping "caramel" SUND-3))
(define SUND-5 (make-topping "syrup" IC8))
;stack-temp: Stack -> ???
#;
(define (stack-temp stack)
  (list-temp (stack-scoops stack)))
(define STACK-1 (list IC3))
(define STACK-2 (list IC3 IC4 IC5 SWIRL1))
(define STACK-3 (list IC5 IC8 IC9 (make-topping "syrup" IC3)))
;list-temp: [Listof IceCream] -> ???
#;
(define (list-temp loc)
  (cond [(empty? (rest loc)) ...]
        [else (... (ice-cream-temp (first loc))
                   (list-temp (rest loc)))]))
;topping-temp: Topping -> ???
#;
(define (topping-temp topping)
  (... (topping-type topping)
       ... (ice-cream-temp (topping-ic topping))))
(define TOP-1 (make-topping "caramel" IC3))
(define TOP-2 (make-topping "syrup" IC4))
(define TOP-3 (make-topping "fruit" IC8))
(define TOP-4 (make-topping "oreo" IC10))

;Exercise 2
;count-flavors: IceCream -> Nat
;produce the total number of flavors in the given IceCream
(check-expect (count-flavors IC1) 1)
(check-expect (count-flavors IC7) 4)
(check-expect (count-flavors IC10) 11)
(check-expect (count-flavors IC8) 12)
(define (count-flavors icecream)
  (local [;count-swirl: Swirl -> Nat
          ;count the number of flavors in a swirl
          (define (count-swirl sw)
            (+ (count-flavors (swirl-type1 sw))
               (count-flavors (swirl-type2 sw))))
          ;count-sundae: Sundae -> Nat
          ;count the number of flavors in Sundaes
          (define (count-sundae sundae)
            (cond [(stack? sundae) (count-stack sundae)]
                  [(topping? sundae) (count-topping sundae)]))
          ;count-stack: Sundae -> Nat
          ;count the number of flavors in Sundaes (stack)
          (define (count-stack stack)
            (count-list (stack-scoops stack)))
          ;count-list: [Listof IceCream] -> Nat
          ;count the number of flavors in [Listof IceCream]
          (define (count-list loc)
            (foldr (λ (x y) (+ (count-flavors x) y)) 0 loc))
          ;count-topping: Sundae -> Nat
          ;count the number of flavors in Sundaes (toppings)
          (define (count-topping topping)
            (count-flavors (topping-ic topping)))]
    (cond [(string? icecream) 1]
          [(swirl? icecream) (count-swirl  icecream)]
          [else (count-sundae icecream)])))

;Exercise 3
;flavor-exists?: IceCream String -> Boolean
;Does the given IceCream contain the given flavor?
(check-expect (flavor-exists? IC10 "vanilla") #t)
(check-expect (flavor-exists? IC1 "coco") #f)
(check-expect (flavor-exists? IC10 "coco") #f)
(check-expect (flavor-exists? IC7 "syrup") #f)
(define (flavor-exists?  icecream string)
  (local [;swirl-exists? : Swirl -> Boolean
          ;Does the swirl contain the given flavor?
          (define (swirl-exists?  sw)
            (or (flavor-exists? (swirl-type1 sw) string)
                (flavor-exists? (swirl-type2 sw) string)))
          ;sundae-exists?: Sundae -> Boolean
          ;Does the Sundae contain the given flavor?
          (define (sundae-exists? sundae)
            (cond [(stack? sundae) (stack-exists? sundae)]
                  [(topping? sundae) (topping-exists? sundae)]))
          ;stack-exists?: Sundae -> Boolean 
          ;Does the Sundae contain the given flavor? (stack)
          (define (stack-exists? stack)
            (list-exists? (stack-scoops stack)))
          ;list-exists?: [Listof IceCream] -> Boolean 
          ;Does the [Listof IceCream] contain the given flavor?
          (define (list-exists? loc)
            (ormap (λ (x) (flavor-exists? x string)) loc))
          ;topping-exists?: Sundae -> Boolean
          ;Does the Sundae contain the given flavor? (toppings)
          (define (topping-exists? topping)
            (flavor-exists? (topping-ic topping) string))]
    (cond [(string? icecream) (string=? string icecream)]
          [(swirl? icecream) (swirl-exists?  icecream)]
          [else (sundae-exists? icecream)])))

;Exercise 4
;remove-topping: IceCream String -> IceCream
;remove the topping of the icecream, if that icecream have the given topping
(check-expect (remove-topping IC10 "syrup")
              (make-stack
               (list "vanilla" "strawberry" "banana"
                     (make-swirl
                      (make-stack
                       (list "vanilla" "strawberry" "vanilla")) "vanilla")
                     (make-stack (list (make-swirl "vanilla" "strawberry") "vanilla")) "vanilla")))
(check-expect (remove-topping IC9 "syrup") (make-stack
                                            (list
                                             "vanilla"
                                             "strawberry"
                                             "banana"
                                             "lemon"
                                             (make-swirl "vanilla" "strawberry")
                                             (make-swirl "vanilla" "strawberry")
                                             (make-swirl "banana" "chocolate")
                                             (make-swirl "lemon" "strawberry"))))
(check-expect (remove-topping IC10 "caramel") IC10)
(check-expect (remove-topping IC1 "caramel") IC1)
(define (remove-topping icecream string)
  (local [;remove-swirl: Swirl -> Swirl 
          ;Remove the topping of the icecream in the swirl with the given topping 
          (define (remove-swirl  sw)
            (make-swirl (remove-topping (swirl-type1 sw) string)
                        (remove-topping (swirl-type2 sw) string)))
          ;remove-sundae: Sundae -> Sundae
          ;Remove the topping of the icecream in the Sundae with the given topping 
          (define (remove-sundae sundae)
            (cond [(stack? sundae) (remove-stack sundae)]
                  [(topping? sundae) (remove-topping2 sundae)]))
          ;remove-stack: Sundae -> Sundae 
          ;Remove the topping of the icecream in the Sundae with the given topping (stack)
          (define (remove-stack stack)
            (make-stack (remove-list (stack-scoops stack))))
          ;remove-list: [Listof IceCream] -> [Listof IceCream] 
          ;;Remove the topping of the icecream in the [Listof IceCream] with the given topping 
          (define (remove-list loc)
            (map (λ (x) (remove-topping x string)) loc))
          ;remove-topping: Sundae -> Sundae
          ;Remove the topping of the icecream in the Sundae with the given topping (toppings)
          (define (remove-topping2 topping)
            (if (string=? (topping-type topping) string)
                (remove-topping (topping-ic topping) string)
                topping))] 
    (cond [(string? icecream) icecream]
          [(swirl? icecream) (remove-swirl icecream)]
          [else (remove-sundae icecream)])))

;Exercise 5
(define-struct object [key value next])
; A JSONObject is one of:
; - (make-object String JSONValue '())
; - (make-object String JSONValue JSONObject)
; Interpretation: A set of key-value pairs, where the
; first option represents an object with only a single
; key-value pair.
;object-temp: JSONObject -> ???
#;
(define (object-temp obj)
  (cond [(empty? (object-next obj))
         (... (object-key obj) ... (jsonvalue-temp (object-value obj)))]
        [else (... (object-key obj)
                   ... (jsonvalue-temp (object-value obj))
                   ... (object-temp (object-next obj)))]))
(define OB1 (make-object "foobar" "foo" '()))
(define OB2 (make-object "foo" (list 1 2 3 "nat") (make-object "bar" 5 '())))

; A JSONValue is one of:
; - Integer
; - String    (that does not contain double-quotes ["])
; - Boolean
; - JSONObject
; - JSONList
;jsonvalue-temp: JSONValue -> ???
#;
(define (jsonvalue-temp jsv)
  (cond [(integer? jsv) ...]
        [(string? jsv) ...]
        [(boolean? jsv) ...]
        [(object? jsv) (... (object-temp jsv))]
        [else (... (jsonlist-temp jsv))]))
(define JSV-1 1)
(define JSV-2 "foo")
(define JSV-3 #t)
(define JSV-4 (make-object "foo" (list 1 "nat")
                (make-object "bar" (make-object "baz" #true '())
                  (make-object "blah" 3 '()))))

; A JSONList is a [List-of JSONValue]
; Interpretation: A list of values
;jsonlist-temp: [List-of JOSNValue] -> ???
#;
(define (jsonlist-temp jsl)
  (cond [(empty? (rest jsl)) ...]
        [else (... (jsonvalue-temp (first jsl))
                   (jsonlist-temp (rest jsl)))]))
(define JSL-1 (list JSV-1 JSV-2))
(define JSL-2 (list JSV-4 JSL-1))

;jsonify: JSONValue -> String
;produces a JSON-formatted String that represents the given JSONValue
(check-expect (jsonify (make-object "cat" 3 (make-object "goat" 1 '())))
              "{\"cat\": 3, \"goat\": 1}")
(check-expect (jsonify (make-object "cat" (list 3 #t #f) (make-object "goat" 1 '())))
              "{\"cat\": [3, true, false], \"goat\": 1}")  
(check-expect (jsonify (make-object "foo" (list 1 "nat")
                         (make-object "bar" (make-object "baz" #true '())
                           (make-object "blah" 3 '()))))
              "{\"foo\": [1, \"nat\"], \"bar\": {\"baz\": true}, \"blah\": 3}")
(define (jsonify jsv)
  (cond [(integer? jsv) (number->string jsv)]
        [(string? jsv) (string-append "\"" jsv "\"")]
        [(boolean? jsv) (if jsv "true" "false")]
        [(object? jsv) (string-append "{" (objectify jsv) "}")]
        [else (string-append "[" (jsonlist jsv) "]")]))

;jsonlist: JSONList -> String
;produces a JSON-formatted String that represents the given JSONList
(check-expect (jsonlist JSL-1) "1, \"foo\"")
(check-expect (jsonlist JSL-2)
              "{\"foo\": [1, \"nat\"], \"bar\": {\"baz\": true}, \"blah\": 3}, [1, \"foo\"]")
(define (jsonlist jsl)
  (cond [(empty? (rest jsl)) (jsonify (first jsl))]
        [else (string-append (jsonify (first jsl))
                             ", " (jsonlist (rest jsl)))]))

;objectify: JSONObject -> String
;produces a JSON-formatted String that represents the given JSONObject
(check-expect (objectify OB1)
              "\"foobar\": \"foo\"")
(check-expect (objectify OB2)
              "\"foo\": [1, 2, 3, \"nat\"], \"bar\": 5")
(define (objectify obj)
  (cond [(empty? (object-next obj))
         (string-append "\"" (object-key obj) "\"" ": " (jsonify (object-value obj)))]
        [else (string-append "\"" (object-key obj) "\"" ": "
                             (jsonify (object-value obj)) ", "
                             (objectify (object-next obj)))]))

