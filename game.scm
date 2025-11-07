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
             (rallisp object)
             (rallisp car))

(define *state* 'running)

(define obj (make-object (vec2 100 100) 16.0 (vec2 10 0) 0 (make-image "assets/images/car.png")))
(define car (make-car obj))
(set-car-acceleration! car (vec2 0 1))
(log (format #f "foo: ~a\n" (object-x obj)))

;; Game data
(define game-width    640.0)
(define game-height   480.0)
(define game-turn 1000.0)
(define *current-turn* 0.0)

(define dt (/ 1000.0 60.0)) ; aim for updating at 60Hz
(define (update)
  (match *state*
    ('running
     (set! *current-turn* (+ dt *current-turn*))
     (car-update! car (* .001 dt))
     (set-object-rotation! obj (+ (object-rotation obj) 0.01))
     (when (> *current-turn* game-turn)
         (set! *current-turn* 0)
         (set! *state* 'prompt))
     )
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

  (when (eq? *state* 'prompt)
    (let* ([speed (object-speed obj)]
           [speed (vec2-mul-scalar speed 10)]
           [x (object-center-x obj)]
           [y (object-center-y obj)]
           [new-x (+ x (vec2-x speed))]
           [new-y (+ y (vec2-y speed))])
      (draw-line context 3 "red" x y pos-x pos-y)
      (draw-line context 3 "blue" x y new-x new-y)))

         
    
  (car-draw car context)
    
  ;; Print score
  (set-fill-color! context "#ffffff")
  (set-font! context "bold 24px monospace")
  (set-text-align! context "left")
  (fill-text context (format #f "~a" *current-turn*) 300 300)
  (request-animation-frame draw-callback))
(define draw-callback (procedure->external draw))

;; Input
(define key:left "ArrowLeft")
(define key:right "ArrowRight")
(define key:confirm "Enter")

(define pos-x 0)
(define pos-y 0)

(define (on-move event)
  (let* ([offset-x (bounding-client-x canvas)]
         [offset-y (bounding-client-y canvas)]
         [x (- (mouse-x event) offset-x)]
         [y (- (mouse-y event) offset-y)])
    (set! pos-x x)
    (set! pos-y y)))
  
(define (on-click event)
  (log (format #f "~a ~a, ~a\n" (mouse-button event) (mouse-x event) (mouse-y event)))
  (log (format #f "~a, ~a\n" (bounding-client-x canvas) (bounding-client-y canvas)))
  (let* ([offset-x (bounding-client-x canvas)]
         [offset-y (bounding-client-y canvas)]
         [x (- (mouse-x event) offset-x)]
         [y (- (mouse-y event) offset-y)])
    (log (format #f "~a ~a\n" x y)))
  (match *state*
    ('running
     #f)
    ('prompt
     (set! *current-turn* 0.0)
     (set! *state* 'running))
    (_ #f)))


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
