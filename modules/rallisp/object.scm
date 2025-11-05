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
  #:use-module (hoot errors)
  #:export (<object>
            object?
            object-hitbox
            object-pos
            object-rotation
            set-object-rotation!
            set-object-speed!
            object-image
            make-object))

(define-record-type <object>
  (%make-object circle speed rotation image)
  object?
  (circle object-hitbox)
  (speed object-speed set-object-speed!)
  (rotation object-rotation set-object-rotation!)
  (image object-image))


(define (make-object pos radius speed rotation image)
  (unless (vec2? pos) (make-assertion-error "Not a vec2" pos))
  (unless (vec2? speed) (make-assertion-error "Not a vec2" speed))
  (%make-object (make-circle pos radius) speed rotation image))

(define (object-pos o)
  (circle-pos (object-circle o)))
