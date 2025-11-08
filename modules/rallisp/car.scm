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
  #:use-module (rallisp track)
  #:use-module (rallisp surface)
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
            car-max-brake
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
                   (max-steer .7)
                   (wheel-base .7))
                              
  (%make-car object .0 .0 max-acceleration max-brake max-steer wheel-base))

(define (car-rotation c)
  (object-rotation (car-object c)))

(define (car-update! c dt track)
  (let* ([o (car-object c)]
         [wb (* (car-wheel-base c) (object-radius o))]
         [steer (car-steer c)]
         [pos-x (object-center-x o)]
         [pos-y (object-center-y o)]
         [surface (track-surface track (vec2 pos-x pos-y))]
         [speed (vec2-mul-scalar
                 (object-speed o)
                 (- 1.0 (* dt (surface-rr surface))))]
         [heading (vec2-normalize speed)]
         [rot (object-rotation o)]
         [power (* (car-acceleration c) dt (surface-grip surface))]
         [accel-rear (vec2-mul-scalar (vec2-normalize speed)
                                      power)]
         [accel-front accel-rear]
         [pos-rear (vec2-sub (vec2 pos-x pos-y)
                             (vec2-mul-scalar heading wb))]
         [pos-front (vec2-add (vec2 pos-x pos-y)
                              (vec2-mul-scalar heading wb))]
         [v-rear (vec2-add speed accel-rear)]
         [v-front (vec2-add speed accel-front)]
         [v-front (vec2-rotate v-front
                               steer)]
         [v-front (vec2-mul-scalar v-front (cos steer))]
         [new-pos-rear (vec2-add pos-rear
                             (vec2-mul-scalar v-rear
                                              dt))]
         [new-pos-front (vec2-add pos-front
                              (vec2-mul-scalar v-front dt))]
                             
         [new-heading (vec2-normalize
                       (vec2-sub new-pos-front
                                 new-pos-rear))]
         [new-v (vec2-mul-scalar new-heading
                                 (+ (vec2-magnitude speed)
                                    power))])
                 
         ;; [new-v (vec2-mul-scalar (vec2-sub (vec2-add new-pos-front new-pos-rear)
         ;;                                   (vec2-add pos-front pos-rear))
         ;;                         (/ .5 dt))])
    
    (debug (format #f "steer: ~a\n" steer))
    (debug (format #f "v-front: ~a, ~a\n" (vec2-x v-front) (vec2-y v-front)))
    (debug (format #f "v-rear: ~a, ~a\n" (vec2-x v-rear) (vec2-y v-rear)))
    (debug (format #f "pos-front: ~a, ~a\n" (vec2-x pos-front) (vec2-y pos-front)))
    (debug (format #f "pos-rear: ~a, ~a\n" (vec2-x pos-rear) (vec2-y pos-rear)))
    (debug (format #f "new-pos-front: ~a, ~a\n" (vec2-x new-pos-front) (vec2-y new-pos-front)))
    (debug (format #f "new-pos-rear: ~a, ~a\n" (vec2-x new-pos-rear) (vec2-y new-pos-rear)))
    (debug (format #f "new-v: ~a, ~a\n" (vec2-x new-v) (vec2-y new-v)))
    (set-object-rotation! o (vec2->angle new-heading))
    (set-object-speed! o new-v)))
