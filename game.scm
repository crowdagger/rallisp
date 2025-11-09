;;; Lizzie Crowdagger, 2025.
;;;
;;; Based on the hoot game jam template:
;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(use-modules (dom canvas)
             (dom document)
             (dom element)
             (dom event)
             (dom image)
             (dom media)
             (dom window)
             (hoot ffi)
             (hoot hashtables)
             (ice-9 match)
             (math)
             (math rect)
             (math vector)
             (srfi srfi-9)
             (rallisp track)
             (rallisp object)
             (rallisp surface)
             (rallisp segment)
             (rallisp game)
             (rallisp car)
             (levels level-1))


;; Game data
(define game-width    640.0)
(define game-height   480.0)
(define game-turn 1000.0)
(define *current-turn* 0.0)
(define *in:click?* #f)
(define *in:mouse-x* 0)
(define *in:mouse-y* 0)

(define controller-left 520)
(define controller-top 360)
(define controller-width 100)
(define controller-height 100)
(define controller-right (+ controller-left controller-width))
(define controller-bottom (+ controller-top controller-height))
(define controller-center-x (+ controller-left (/ controller-width 2)))
(define controller-center-y (+ controller-top (/ controller-height 2)))
(define (mouse-in-controller?)
  (and (<= *in:mouse-x* controller-right)
       (<= *in:mouse-y* controller-bottom)
       (>= *in:mouse-x* controller-left)
       (>= *in:mouse-y* controller-top)))

(define (reinit-inputs!)
  "Reunitialize all inputs, at the END of a frame"
  (set! *in:click?* #f))

(define *game* (make-game))


(define obj (make-object (vec2 0 0) 16.0 (vec2 0 0.0) 0 (make-image "assets/images/car.png")))
(define player (make-car obj))
(register-object! *game* obj)
(register-car! *game* player)

(define track (make-level-1))
(set-game-track! *game* track)

  
(let lp ([n 0])
  (when (> n 0)
    (let* ([x (* (random) game-width)]
           [y (* (random) game-height)]
           [spd-x (- 15 (* (random) 30))]
           [spd-y (- 15 (* (random) 30))]
           [particle (make-object (vec2 x y) 4.0 (vec2 spd-x spd-y) 0 (make-image "assets/images/smoke.png"))])
      (register-object! *game* particle)
      (lp (- n 1)))))
          






(define dt (/ 1000.0 60.0)) ; aim for updating at 60Hz
(define (update)
  (match (game-state *game*)
    ('running
     (set! *current-turn* (+ dt *current-turn*))
     (game-update! *game* (* .001 dt))
;     (log (format #f "collision?: ~a\n" (length (game-collisions *game*))))
;     (set-object-rotation! obj (+ (object-rotation obj) 0.01))
     (when (> *current-turn* game-turn)
         (set! *current-turn* 0)
         (set-game-state! *game* 'prompt)))
    (_ #t))
  (timeout update-callback dt))
(define update-callback (procedure->external update))

;; Rendering
(define number->string*
  (let ((cache (make-eq-hashtable))) ; assuming fixnums only
    (lambda (x)
      (or (hashtable-ref cache x)
          (let ((str (number->string x)))
            (hashtable-set! cache x str)
            str)))))

(define (draw prev-time)
  ;; Clear canvas
  (clear-rect context 0.0 0.0 game-width game-height)

  ;; Draw background
  (set-fill-color! context "#006600")
  (fill-rect context 0.0 0.0 game-width game-height)

  (set-game-viewport! *game*
                      (vec2 (- (object-center-x obj) 320)
                            (- (object-center-y obj) 240)))
  (game-draw *game* context)

  (when (eq? (game-state *game*) 'prompt)
    ;; Add transparency to whole screen
    (set-global-alpha! context .2)
    (set-fill-color! context "#FF0000")
    (fill-rect context 0 0 game-width game-height)
    (set-global-alpha! context 1)
    ;; Display input-square
    (set-fill-color! context "#000000")
    (fill-rect context controller-left controller-top controller-width controller-height)
    (draw-line context 3 "#ffffff" controller-left controller-center-y controller-right controller-center-y)
    (draw-line context 3 "#ffffff" controller-center-x controller-top controller-center-x controller-bottom)

    (when (mouse-in-controller?)
      (draw-circle context "#FF0000" *in:mouse-x* *in:mouse-y* 3))
    
    ;; When in prompt state, process inputs
    ;; If there is a click, enter running state
    (when (and *in:click?*
               (mouse-in-controller?))
      (let* ([x (/ (- *in:mouse-x* controller-center-x)
                   (/ controller-width 2))]
             [y (/ (- controller-center-y *in:mouse-y*) ; reverted because y-axis goes down
                   (/ controller-height 2))]
             [power (if (positive? y)
                        (* y (car-max-acceleration player))
                        (* y (car-max-brake player)))]
             [steer (* x (car-max-steer player))])
        
        
        (set-car-acceleration! player power)
        (set-car-steer! player steer)
        (set-game-state! *game* 'running)
        (set! *current-turn* 0))))
         

    
    ;; Print speed
    (set-fill-color! context "#ffffff")
    (set-font! context "bold 12px monospace")
    (set-text-align! context "left")
    (fill-text context (format #f "~a km/h" (car-kmh player)) 20 460)
    (reinit-inputs!)
    (request-animation-frame draw-callback))
(define draw-callback (procedure->external draw))

;; Input
(define key:left "ArrowLeft")
(define key:right "ArrowRight")
(define key:confirm "Enter")

(define (on-click event)
  (set! *in:click?* #t))


(define (on-move event)
  (let* ([offset-x (bounding-client-x canvas)]
         [offset-y (bounding-client-y canvas)]
         [x (- (mouse-x event) offset-x)]
         [y (- (mouse-y event) offset-y)]
         [rel-x (+ x (vec2-x (game-viewport *game*)))]
         [rel-y (+ y (vec2-y (game-viewport *game*)))])
    (debug (format #f "x: ~a, y: ~a, s: ~a\n"
                   rel-x rel-y
                   (surface-name (track-surface track (vec2 rel-x rel-y)))))
    (set! *in:mouse-x* x)
    (set! *in:mouse-y* y)))
  


(define (on-key-down event)
  #f)
  
(define (on-key-up event)
  #f)

;; Canvas and event loop setup
(define canvas (get-element-by-id "canvas"))
(define context (get-context canvas "2d"))

(set-element-width! canvas (inexact->exact game-width))
(set-element-height! canvas (inexact->exact game-height))
(add-event-listener! (current-document) "keydown"
                     (procedure->external on-key-down))
(add-event-listener! (current-document) "keyup"
                     (procedure->external on-key-up))
(add-event-listener! canvas "click" (procedure->external on-click))
(add-event-listener! canvas "mousemove" (procedure->external on-move))

(request-animation-frame draw-callback)
(timeout update-callback dt)
