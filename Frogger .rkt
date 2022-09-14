;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Frogger |) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define BACKGROUND (place-image
                    (rectangle 300 20 "solid" "dark purple")
                    150 10
                    (place-image
                     (rectangle 300 20 "solid" "dark purple")
                     150 130
                     (rectangle 300 140 "solid" "black"))))

(define BLOCK (rectangle 301 141 "outline" "black"))

(define FROG (place-image (circle 8 "solid" "Dark Green") 8.5 8.5 (circle 8.5 "outline" "black")))

(define CAR1 (rectangle 25 11 "solid" "Light Red"))

(define-struct position [x y])

;; A Position is a (make-position Number Number)
;; x coordinate between [0, 300] and y coordinates between [0, 140] `of an object

(define SP11 (make-position 287.5 110))
(define SP12 (make-position 222.5 110))
(define SP13 (make-position 157.5 110))
(define SP14 (make-position 92.5 110))

(define SP21 (make-position 12.5 90))
(define SP22 (make-position 77.5 90))
(define SP23 (make-position 142.5 90))
(define SP24 (make-position 207.5 90))

(define SP31 (make-position 267.5 70))
(define SP32 (make-position 202.5 70))
(define SP33 (make-position 137.5 70))
(define SP34 (make-position 72.5 70))

(define SP41 (make-position 32.5 50))
(define SP42 (make-position 97.5 50))
(define SP43 (make-position 162.5 50))
(define SP44 (make-position 227.5 50))

(define SP51 (make-position 247.5 30))
(define SP52 (make-position 182.5 30))
(define SP53 (make-position 117.5 30))
(define SP54 (make-position 52.5 30))

#;
(define (position-temp p)
  ... (position-x p) ...
  ... (position-y p) ...)

(define-struct vehicle [direction position])

;; A vehicle is (make-vehicle Boolean Position)
;; represents a vehicle
;; direction is a boolean where
;; #true is left (car moving left)
;; #false is right (car moving right)

(define V1 (make-vehicle #true SP11))
(define V2 (make-vehicle #true SP12))
(define V3 (make-vehicle #true SP13))
(define V4 (make-vehicle #true SP14))
(define V5 (make-vehicle #false SP21))
(define V6 (make-vehicle #false SP22))
(define V7 (make-vehicle #false SP23))
(define V8 (make-vehicle #false SP24))
(define V9 (make-vehicle #true SP31))
(define V10 (make-vehicle #true SP32))
(define V11 (make-vehicle #true SP33))
(define V12 (make-vehicle #true SP34))
(define V13 (make-vehicle #false SP41))
(define V14 (make-vehicle #false SP42))
(define V15 (make-vehicle #false SP43))
(define V16 (make-vehicle #false SP44))
(define V17 (make-vehicle #true SP51))
(define V18 (make-vehicle #true SP52))
(define V19 (make-vehicle #true SP53))
(define V20 (make-vehicle #true SP54))

#;
(define (vehicle-temp v)
  ( ... (vehicle-direction v) ...
        ... (position-temp (vehicle-position v)) ... ))
                     
;; A LoV (list of vehicles) is one of:
;; -'()
;; - (cons vehicle LoV)
;; represents a list of vehicles

(define LOV1 '())
(define LOV2 (cons V1 LOV1))
(define LOV3 (cons V2 LOV2))
(define LOV4 (cons V3 LOV3))
(define LOV5 (cons V4 LOV4))
(define LOV6 (cons V5 LOV5))
(define LOV7 (cons V6 LOV6))
(define LOV8 (cons V7 LOV7))
(define LOV9 (cons V8 LOV8))
(define LOV10 (cons V9 LOV9))
(define LOV11 (cons V10 LOV10))
(define LOV12 (cons V11 LOV11))
(define LOV13 (cons V12 LOV12))
(define LOV14 (cons V13 LOV13))
(define LOV15 (cons V14 LOV14))
(define LOV16 (cons V15 LOV15))
(define LOV17 (cons V16 LOV16))
(define LOV18 (cons V17 LOV17))
(define LOV19 (cons V18 LOV18))
(define LOV20 (cons V19 LOV19))
(define LOV21 (cons V20 LOV20))

#;
(define (lov-temp lov)
  (cond [(empty? lov) ....]
        [(cons? lov) ... (vehicle-temp (firt lov))
                     ... (lov-temp (rest lov)) ... ]))
     
;; A FrogPosition is a Position
;; the position of FROG

(define FP1 (make-position 150 130))
(define FP2 (make-position 0 0))
(define FP3 (make-position 1 1))

#;
(define (position-temp p)
  ... (position-x p) ...
  ... (position-y p) ...)

(define-struct world-state [fp lov])

;; world-state is a (make-world-state FrogPosition LoV)
;; represents state of the frogger-world
;; fp represents frog position
;; lov represents List of Vehicles

(define WS1 (make-world-state FP3 LOV1))
(define WS2 (make-world-state FP2 LOV2))
(define WS3 (make-world-state FP1 LOV3))
(define WS4 (make-world-state FP1 LOV21))

#;
(define (world-temp w)
  (... (position-temp (world-state-fp w)) ...
       ... (lov-temp (world-state-lov w)) ...))

;; frogger-world: world-state -> world-state
;; launches the frogger game

(define (frogger-world f)
  (big-bang f
    [to-draw draw-state]
    [on-tick ticker]
    [on-key change-state]
    [stop-when stop-state last-scene]))

;; draw-state : world-state -> Image
;; draws the frogger game based on the position of the frog and the position of the LoV

(define (draw-state ws)
  (overlay (draw-frog (world-state-fp ws))
           (draw-vehicle (world-state-lov ws))))

(check-expect (draw-state WS1) (overlay (place-image FROG 1 1 BLOCK) BACKGROUND))
(check-expect (draw-state WS2) (overlay (place-image FROG 0 0 BLOCK)
                                        (place-image CAR1 287.5 110 BACKGROUND)))
(check-expect (draw-state WS3) (overlay (place-image FROG 150 130 BLOCK)
                                        (place-image CAR1 222.5 110
                                                     (place-image CAR1 287.5 110 BACKGROUND))))

;; draw-frog : FrogPosition -> Image
;; draws FROG at FrogPosition

(define (draw-frog fp)
  (place-image FROG (position-x fp) (position-y fp) BLOCK))

(check-expect (draw-frog FP1) (place-image FROG 150 130 BLOCK))
(check-expect (draw-frog FP2) (place-image FROG 0 0 BLOCK))
(check-expect (draw-frog FP3) (place-image FROG 1 1 BLOCK))
             
;; draw-vehicle : LoV -> Image
;; Draws a list of vehicles

(define (draw-vehicle lov)
  (cond [(empty? lov) BACKGROUND]
        [(cons? lov) (place-image CAR1 (position-x (vehicle-position (first lov)))
                                  (position-y (vehicle-position (first lov)))
                                  (draw-vehicle (rest lov)))]))

(check-expect (draw-vehicle LOV1) BACKGROUND)
(check-expect (draw-vehicle LOV2) (place-image CAR1 287.5 110 BACKGROUND))
(check-expect (draw-vehicle LOV3) (place-image CAR1 222.5 110
                                               (place-image CAR1 287.5 110 BACKGROUND)))

;; ticker : World-State --> World-State
;; Advances world state

(define (ticker ws)
  (make-world-state (world-state-fp ws) (list-helper (world-state-lov ws))))

(check-expect (ticker WS1) (make-world-state FP3 LOV1))
(check-expect (ticker WS2) (make-world-state FP2 (cons (make-vehicle #true (make-position 286.5 110))
                                                       LOV1)))
(check-expect (ticker WS3) (make-world-state FP1
                                             (cons
                                              (make-vehicle #true (make-position 221.5 110))
                                              (cons
                                               (make-vehicle #true
                                                             (make-position 286.5 110))
                                               LOV1))))

;; list-helper : LoV --> LoV
;; Changes list of vehicles depending on direction

(define (list-helper lov)
  (map vehicle-helper lov))

(check-expect (list-helper LOV1) '())
(check-expect (list-helper LOV2) (cons (make-vehicle #true (make-position 286.5 110))
                                       LOV1))
(check-expect (list-helper LOV3) (cons
                                  (make-vehicle #true (make-position 221.5 110))
                                  (cons
                                   (make-vehicle #true
                                                 (make-position 286.5 110))
                                   LOV1)))

;; vehicle-helper : Vehicle --> Vehicle
;; Changes a vehicle's position

(define (vehicle-helper v)
  (if (vehicle-direction v)
      (make-vehicle (vehicle-direction v) (if (< (position-x (vehicle-position v)) 12.5)
                                              (make-position 287.5 (position-y (vehicle-position v)))
                                              (make-position (- (position-x (vehicle-position v)) 1)
                                                             (position-y (vehicle-position v)))))
      (make-vehicle (vehicle-direction v) (if (> (position-x (vehicle-position v)) 287.5)
                                              (make-position 12.5 (position-y (vehicle-position v)))
                                              (make-position (+ (position-x (vehicle-position v)) 1)
                                                             (position-y (vehicle-position v)))))))

(check-expect (vehicle-helper V1) (make-vehicle #true (make-position 286.5 110)))
(check-expect (vehicle-helper V2) (make-vehicle #true (make-position 221.5 110)))
(check-expect (vehicle-helper V5) (make-vehicle #false (make-position 13.5 90)))
(check-expect (vehicle-helper (make-vehicle #true (make-position 12 70)))
              (make-vehicle #true (make-position 287.5 70)))
(check-expect (vehicle-helper (make-vehicle #false (make-position 288 70)))
              (make-vehicle #false (make-position 12.5 70)))

;; change-state : World-State KeyEvent -> World-State
;; creates new world state when a key is pressed

(define (change-state ws key)
  (make-world-state (change-key (world-state-fp ws) key) (world-state-lov ws)))

(check-expect (change-state WS2 "right") (make-world-state (make-position 7 0) LOV2))
(check-expect (change-state WS2 "down") (make-world-state (make-position 0 7) LOV2))
(check-expect (change-state WS3 "up") (make-world-state (make-position 150 123) LOV3))
(check-expect (change-state WS4 "left") (make-world-state (make-position 143 130) LOV21))

;; change-key : FrogPosition KeyEvent -> FrogPosition
;; changes frog position when one of these keys are pressed:
;; - "up"
;; - "right"
;; - "left"
;; - "down"

(check-expect (change-key FP1 "right" ) (make-position 157 130))
(check-expect (change-key FP1 "left") (make-position 143 130))
(check-expect (change-key FP1  "up") (make-position 150 123))
(check-expect (change-key FP1 "down") (make-position 150 137))

(check-expect (change-key (make-position 150 135) "down") (make-position 150 135))
(check-expect (change-key (make-position 150 5) "up") (make-position 150 5))
(check-expect (change-key (make-position 297 135) "right") (make-position 297 135))
(check-expect (change-key (make-position 3 135) "left") (make-position 3 135))

(define (change-key fp key)
  (cond
    [(key=? key "right") (if (>= (position-x fp) 292) fp
                             (make-position (+ (position-x fp) 7) (position-y fp)))]
    [(key=? key "left") (if (<= (position-x fp) 8) fp
                            (make-position (- (position-x fp) 7) (position-y fp)))]
    [(key=? key "up") (if (<= (position-y fp) 8) fp
                          (make-position (position-x fp) (- (position-y fp) 7)))]
    [(key=? key "down") (if (>= (position-y fp) 132) fp
                            (make-position (position-x fp) (+ (position-y fp) 7)))]))

;; stop-state : WorldState -> Boolean
;; stops game if frog position equals car position

(define (stop-state ws)
  (local-helper (world-state-fp ws) (world-state-lov ws)))

(check-expect (stop-state (make-world-state (make-position 286.5 110)
                                            (cons (make-vehicle #true (make-position 287.5 110))
                                                  LOV1))) #true)
(check-expect (stop-state WS1) #false)

; DO THIS: MAKE TESTS FOR STOP-HELPER AND CHECK SIGNATURE AND PURPOSE STATEMENT

;; stop-helper : FP LoV -> Boolean
;; Checks if the Frog is touching any car from either side or from the top or bottom
;; true means they are touching, false means they are not

; local-helper : FP LoV --> Boolean
; Is a frog touching any car?

(define (local-helper fp lov)
  (local [(define (stop-helper lov)
            (or (right-car fp lov)
                (left-car fp lov)
                (above-car fp lov)
                (below-car fp lov)))]
    (ormap stop-helper lov)))

(check-expect (local-helper (make-position 287.5 170)
                            (cons (make-vehicle #true (make-position 287.5 175.5)) LOV1)) #t)
(check-expect (local-helper (make-position 287.5 185.5)
                            (cons (make-vehicle #true (make-position 287.5 180)) LOV1)) #t)

(check-expect (local-helper (make-position 290 140.5)
                            (cons (make-vehicle #true (make-position 130 110)) LOV1)) #f)

;; right-car : FP Vehicle -> Boolean
;; Checks if Frog is touching a vehicle on the right

(define (right-car fp f)
  (coordinate-checker fp f 0 20.5 13.5 -13.5))

(check-expect (right-car (make-position 282.5 120) (make-vehicle #true (make-position 270.5 110))) #t)
(check-expect (right-car (make-position 280.5 110) (make-vehicle #true (make-position 288 110))) #f)

;; left-car : FP Vehicle -> Boolean
;; Checks if Frog is touching a vehicle on the left

(define (left-car fp f)
  (coordinate-checker fp f -20.5 0 13.5 -13.5))

(check-expect (left-car (make-position 270 120) (make-vehicle #true (make-position 282.5 110))) #t)
(check-expect (left-car (make-position 287.5 110) (make-vehicle #true (make-position 280 110))) #f)

;; above-car : FP Vehicle -> Boolean
;; Checks if Frog is touching a vehicle on top

(define (above-car fp f)
  (coordinate-checker fp f -20.5 20.5 0 -13.5))

(check-expect (above-car (make-position 280 180)
                         (make-vehicle #true (make-position 290 175.5))) #f)
(check-expect (above-car (make-position 287.5 110)
                         (make-vehicle #true (make-position 287.5 110))) #f)

;; below-car : FP Vehicle -> Boolean
;; Checks if Frog is touching a vehicle on bottom

(define (below-car fp f)
  (coordinate-checker fp f -20.5 20.5 13.5 0))

(check-expect (below-car (make-position 290 185.5)
                         (make-vehicle #true (make-position 280 180))) #t)
(check-expect (below-car (make-position 287.5 110)
                         (make-vehicle #true (make-position 287.5 110))) #f)

; coordinate-checker : FP Vehicle Number Number Number Number --> Boolean
; Checks if difference in coordinates fit within a specified range

(define (coordinate-checker fp f x-diff-r x-diff-l y-diff-t y-diff-b)
  (and (and (> (- (position-x fp) (position-x (vehicle-position f))) x-diff-r)
                (< (- (position-x fp) (position-x (vehicle-position f))) x-diff-l))
           (and (< (- (position-y fp) (position-y (vehicle-position f))) y-diff-t)
                (> (- (position-y fp) (position-y (vehicle-position f))) y-diff-b))))

(check-expect (coordinate-checker (make-position 290 185.5)
                         (make-vehicle #true (make-position 280 180)) -20.5 20.5 13.5 0) #t)

(check-expect (coordinate-checker (make-position 270 120)
                        (make-vehicle #true (make-position 282.5 110)) -20.5 0 13.5 -13.5) #t)

;; last-scene : WS --> Image
;; Draws final scene of game

(define (last-scene ws)
  (overlay (text "GAME OVER" 25 "white")
           (rectangle 300 140 "solid" "black")))

(check-expect (last-scene WS1) (overlay (text "GAME OVER" 25 "white")
                                        (rectangle 300 140 "solid" "black")))
(check-expect (last-scene WS2) (overlay (text "GAME OVER" 25 "white")
                                        (rectangle 300 140 "solid" "black")))
(check-expect (last-scene WS3) (overlay (text "GAME OVER" 25 "white")
                                        (rectangle 300 140 "solid" "black")))
