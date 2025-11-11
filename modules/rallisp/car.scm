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
            car-kmh
            car-turning-radius))

(define min-speed .1)

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
                   (max-acceleration 100)
                   (max-brake 100)
                   (max-steer .03)
                   (wheel-base .7))
                              
  (%make-car object .0 .0 max-acceleration max-brake max-steer wheel-base))

(define (car-kmh c)
  "Return a vaguely remotely plausible speed in km/h"
  (let ([spd (vec2-magnitude (object-speed (car-object c)))]
        [pixel-per-meter 15])
    (inexact->exact (round (* (/ 3.6 15)
                              spd)))))

(define (car-rotation c)
  (object-rotation (car-object c)))

(define (car-turning-radius c)
  (let ([alpha (/ (car-steer c) 2)]
        [wb (* (car-wheel-base c) (object-radius (car-object c)))])
    (if (zero? alpha)
        #f
        (/ wb (tan alpha)))))

(define (car-update! c dt track)
  (let* ([o (car-object c)]
         [wb (* (car-wheel-base c) (object-radius o))]
         [spd-scalar (vec2-magnitude (object-speed o))]
         [steer (/ (car-steer c)
                   (+ 1 (/ spd-scalar 100)))]
         [pos-x (object-center-x o)]
         [pos-y (object-center-y o)]
         [surface (track-surface track (vec2 pos-x pos-y))]
         [speed (vec2-mul-scalar
                 (object-speed o)
                 (- 1.0 (* dt (surface-rr surface))))]
         [heading (vec2-rotate (vec2 1 0)
                               (car-rotation c))]
         [power (* (car-acceleration c) dt (surface-grip surface))]
         [new-magnitude (+ power  (vec2-magnitude speed))]
         [accel-rear (vec2-mul-scalar heading
                                      power)]
         [accel-front accel-rear]
         [pos-rear (vec2-sub (vec2 pos-x pos-y)
                             (vec2-mul-scalar heading wb))]
         [pos-front (vec2-add (vec2 pos-x pos-y)
                              (vec2-mul-scalar heading wb))]
         [v-rear (vec2-add
                  accel-rear
                  (vec2-add speed
                            (vec2-mul-scalar
                             (vec2-rotate heading
                                          (* .5 pi))
                             (* (surface-grip surface)
                                spd-scalar
                                (clamp (sin (- (car-rotation c)
                                               (vec2->angle speed)))
                                       -.02
                                       .02)))))]
         [v-front (vec2-rotate v-rear
                               steer)]
         [new-pos-rear (vec2-add pos-rear
                                 v-rear)]
         [new-pos-front (vec2-add pos-front
                                  v-front)]
         [new-heading (if (and (= (vec2-x new-pos-front)
                                  (vec2-x new-pos-rear))
                               (= (vec2-y new-pos-front)
                                  (vec2-y new-pos-rear)))
                          heading
                          (vec2-normalize
                           (vec2-sub new-pos-front
                                     new-pos-rear)))]
         [new-v (vec2-mul-scalar (vec2-add
                                  (vec2-sub new-pos-front pos-front)
                                  (vec2-sub new-pos-rear pos-rear))
                                (/ .5 1))])
    (debug (format #f "pos-front: ~a, ~a\n" (vec2-x pos-front) (vec2-y pos-front)))
    (debug (format #f "pos-rear: ~a, ~a\n" (vec2-x pos-rear) (vec2-y pos-rear)))
    (debug (format #f "new-pos-front: ~a, ~a\n" (vec2-x new-pos-front) (vec2-y new-pos-front)))
    (debug (format #f "new-pos-rear: ~a, ~a\n" (vec2-x new-pos-rear) (vec2-y new-pos-rear)))
    (debug (format #f "new-magnitude: ~a\n" new-magnitude))
    (debug (format #f "new-heading: ~a, ~a\n" (vec2-x new-heading) (vec2-y new-heading)))
    (debug (format #f "new-v: ~a, ~a\n" (vec2-x new-v) (vec2-y new-v)))
    (unless (< new-magnitude min-speed)
      (set-object-rotation! o (vec2->angle new-heading)))
    (set-object-speed! o new-v)))
