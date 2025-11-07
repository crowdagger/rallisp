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
            process-car-input))

(define-record-type <car>
  (%make-car object acceleration steer max-acceleration max-brake max-steer)
  car?
  (object car-object)
  (acceleration car-acceleration set-car-acceleration!)
  (steer car-steer set-car-steer!)
  (max-acceleration car-max-acceleration)
  (max-brake car-max-brake)
  (max-steer car-max-steer))

(define* (make-car object #:optional (max-acceleration 50) (max-brake 100) (max-steer 1.0))
  (%make-car object .0 .0 max-acceleration max-brake max-steer))

(define (car-rotation c)
  (object-rotation (car-object c)))

(define (car-update! c dt)
  (let* ([o (car-object c)]
         [acceleration (* (car-acceleration c) dt)]
         [angle-rear (car-rotation c)]
         [angle-front (+ (car-steer c) angle-rear)]
         [dv (vec2-rotate (vec2 1 0)
                          angle-front)]
         [dv (vec2-mul-scalar dv acceleration)]
         [speed (object-speed o)])
    (debug (format #f "acceleration: ~a (~a)\n" acceleration (car-acceleration c)))
    (debug (format #f "dv: ~a, ~a\n" (vec2-x dv) (vec2-y dv)))
    (debug (format #f "speed: ~a, ~a\n" (vec2-x speed) (vec2-y speed)))

    (set-object-rotation! o (vec2->angle speed))
    (set-object-speed! o (vec2-add speed dv))))

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
         [v-est (vec2 (- pos-x new-x) (- pos-y new-y))]
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
