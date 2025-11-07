;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
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

;;; Commentary:
;;;
;;; Circles, ie. a vector and a radius.
;;;
;;; Code:

(define-module (math circle)
  #:use-module (math)
  #:use-module (math vector)
  #:use-module (srfi srfi-9)
  #:export (<circle>
            make-circle
            circle-pos
            set-circle-pos!
            circle-radius
            set-circle-radius!
            circles-collision?))

;; Should we use bytevectors? Probably. Whatever.
(define-record-type <circle>
  (make-circle pos radius)
  circle?
  (pos circle-pos set-circle-pos!)
  (radius circle-radius set-circle-radius!))

;; Test whether two circles are colliding
(define (circles-collision? c1 c2)
  (let* ([dist (vec2-sub (circle-pos c1) (circle-pos c2))]
         [dist (vec2-magnitude dist)]
         [r-sum (+ (circle-radius c1) (circle-radius c2))])
    (<= dist r-sum)))
