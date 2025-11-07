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
  #:use-module (srfi srfi-9)
  #:export (<car>
            car?
            make-car
            car-object
            car-acceleration
            set-car-acceleration!
            car-update!
            car-draw))

(define-record-type <car>
  (%make-car object acceleration)
  car?
  (object car-object)
  (acceleration car-acceleration set-car-acceleration!))

(define (make-car object)
  (let ([acceleration (vec2 0 0)])
    (%make-car object acceleration)))

(define (car-draw c context)
  (object-draw (car-object c) context))

(define (car-update! c dt)
  (let* ([o (car-object c)]
         [acceleration (vec2-mul-scalar (car-acceleration c) dt)]
         [speed (object-speed o)])
    (set-object-speed! o (vec2-add speed acceleration))
    (object-update! o dt)))
