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

;;; Basic object that can be rendered on screen

(define-module (rallisp object)
  #:use-module (srfi srfi-9)
  #:use-module (math circle)
  #:use-module (math vector)
  #:use-module (hoot errors)
  #:use-module (dom canvas)
  #:export (<object>
            object?
            object-hitbox
            object-pos
            object-x
            object-y
            object-center-x
            object-center-y
            object-radius
            object-rotation
            set-object-rotation!
            set-object-pos!
            set-object-speed!
            object-image
            object-speed
            object-draw
            object-update!
            object-collides?
            make-object))

(define-record-type <object>
  (%make-object circle speed rotation image)
  object?
  (circle object-hitbox)
  (speed object-speed set-object-speed!)
  (rotation object-rotation set-object-rotation!)
  (image object-image))


(define (make-object pos radius speed rotation image)
  (%make-object (make-circle pos radius) speed rotation image))

(define (set-object-pos! o pos)
  (set-circle-pos! (object-hitbox o) pos))

(define (object-pos o)
  (circle-pos (object-hitbox o)))

(define (object-radius o)
  (circle-radius (object-hitbox o)))

(define (object-x o)
  (vec2-x (object-pos o)))

(define (object-y o)
  (vec2-y (object-pos o)))

(define (object-center-x o)
  (+ (object-x o)
     (object-radius o)))

(define (object-center-y o)
  (+ (object-y o)
     (object-radius o)))


(define (object-draw o context viewport)
  "Draw object o on context"
  (let* ([r (object-radius o)]
         [d (* 2 r)]
         [rot (object-rotation o)]
         [x-v (- (vec2-x viewport))]
         [y-v (- (vec2-y viewport))]
         [x (object-x o)]
         [y (object-y o)])
    (save! context)
    (set-filter! context "drop-shadow(-5px 4px 3px #222222")

    (set-transform! context 1.0 0 0 1.0 (+ x r x-v) (+ y r y-v))
    (rotate! context rot)
    
    (draw-image context (object-image o)
                0.0 0.0 d d 
                (- r) (- r) d d)
    (set-transform! context 1 0 0 1 0 0)
    (restore! context)))

(define (object-update! o dt)
  "Update object physically"
  ; Add speed to pos
  (set-object-pos! o (vec2-add (object-pos o)
                               (vec2-mul-scalar (object-speed o) dt))))


(define (object-collides? o other)
  (if (eq? o other)
      #f
      (circles-collision? (object-hitbox o) (object-hitbox other))))
