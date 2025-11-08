;;; Copyright (C) 2025 Lizzie Crowdagger <lizzie@crowdagger.fr>
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

(define-module (rallisp car)
  #:use-module (rallisp object)
  #:use-module (math vector)
  #:use-module (dom canvas)
  #:use-module (dom document)
  #:use-module (math)
  #:use-module (srfi srfi-9)
  #:export (<car>
            car?
            make-car
            car-object
            car-acceleration
            car-rotation
            set-car-acceleration!
            car-update!
            car-steer
            set-car-steer!
            car-max-acceleration
            car-max-steer
            car-wheel-base
            process-car-input))

(define-record-type <car>
  (%make-car object acceleration steer max-acceleration max-brake max-steer wheel-base)
  car?
  (object car-object)
  (acceleration car-acceleration set-car-acceleration!)
  (steer car-steer set-car-steer!)
  (max-acceleration car-max-acceleration)
  (wheel-base car-wheel-base)
  (max-brake car-max-brake)
  (max-steer car-max-steer))

(define* (make-car object #:optional
                   (max-acceleration 30)
                   (max-brake 50)
                   (max-steer (* .25 pi))
                   (wheel-base .7))
                              
  (%make-car object .0 .0 max-acceleration max-brake max-steer wheel-base))

(define (car-rotation c)
  (object-rotation (car-object c)))

(define (car-update! c dt)
  (let* ([o (car-object c)]
         [r (object-radius o)]
         [wb (car-wheel-base c)]
         [steer (car-steer c)]
         [speed (object-speed o)]
         [pos-x (object-center-x o)]
         [pos-y (object-center-y o)]
         [rot (object-rotation o)]
         [power (* (car-acceleration c) dt)]
         [accel-rear (vec2-mul-scalar (vec2-normalize speed)
                                      power)]
         [accel-front accel-rear]
         [pos-rear (vec2 (- pos-x (* r wb))
                         pos-y)]
         [pos-front (vec2 (+ pos-x (* r wb))
                         pos-y)]
         [v-rear (vec2-add speed accel-rear)]
         [v-front (vec2-add speed accel-front)]
         [v-front (vec2-rotate v-front
                               steer)]
         [new-pos-rear (vec2-add pos-rear
                             (vec2-mul-scalar v-rear
                                              dt))]
         [new-pos-front (vec2-add pos-front
                              (vec2-mul-scalar v-front dt))]
                             
         [new-heading (vec2-normalize
                       (vec2-sub new-pos-front
                                 new-pos-rear))]
         [new-v (vec2-mul-scalar (vec2-sub (vec2-add new-pos-front new-pos-rear)
                                           (vec2-add pos-front pos-rear))
                                 (/ .5 dt))])
    
    (debug (format #f "steer: ~a\n" steer))
    (debug (format #f "v-front: ~a, ~a\n" (vec2-x v-front) (vec2-y v-front)))
    (debug (format #f "v-rear: ~a, ~a\n" (vec2-x v-rear) (vec2-y v-rear)))
    (debug (format #f "pos-front: ~a, ~a\n" (vec2-x pos-front) (vec2-y pos-front)))
    (debug (format #f "pos-rear: ~a, ~a\n" (vec2-x pos-rear) (vec2-y pos-rear)))
    (debug (format #f "new-pos-front: ~a, ~a\n" (vec2-x new-pos-front) (vec2-y new-pos-front)))
    (debug (format #f "new-pos-rear: ~a, ~a\n" (vec2-x new-pos-rear) (vec2-y new-pos-rear)))
    (debug (format #f "new-v: ~a, ~a\n" (vec2-x new-v) (vec2-y new-v)))
    (set-object-rotation! o (vec2->angle new-v))
    (set-object-speed! o new-v)))

(define (process-car-input c pos-x pos-y context)
  (let* ([max-angle (car-max-steer c)]
         [max-acc (car-max-acceleration c)]
         [max-brake (car-max-brake c)]
         [obj (car-object c)]
         [speed (object-speed obj)]
;         [speed (vec2-mul-scalar speed 60)]
         [x (object-center-x obj)]
         [y (object-center-y obj)]
         [new-x (+ x (vec2-x speed))]
         [new-y (+ y (vec2-y speed))]
         [v-spd (vec2 (- new-x x) (- new-y y))]
         [v-est (vec2 (- pos-x x) (- pos-y y))]
         [dot (vec2-dot v-spd v-est)]
         [wedge (vec2-wedge v-spd v-est)]
         [mag2 (* (vec2-magnitude v-spd) (vec2-magnitude v-est))]
         [angle (asin (/ wedge mag2))]
         [angle (if (positive? dot)
                    angle
                    (- angle))]
         [valid? (and (< (abs angle) max-angle)
                      (< (vec2-magnitude v-est)
                         (if (positive? dot)
                             max-acc
                             max-brake)))]
         [color (if valid? "green" "red")]
         ; Corrected angles and acceleration
;         [angle (if (< (abs angle) max-angle)
;                    (* max-angle (sign angle)))]
         [acceleration (if (positive? dot)
                           (clamp (vec2-magnitude v-est) 0 max-acc)
                           (* (sign dot)
                              (clamp (vec2-magnitude v-est) 0 max-brake)))])

    (draw-line context 3 color x y pos-x pos-y)
    (draw-line context 3 "blue" x y new-x new-y)
    (cons acceleration angle)))
