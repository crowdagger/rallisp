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


;;; Data on the track surface

(define-module (rallisp surface)
  #:use-module (srfi srfi-9)
  #:export (<surface>
            surface?
            make-surface
            surface-apparence
            surface-rr
            surface-grip
            surf:asphalt
            surf:grass
            surf:sand))

(define-record-type <surface>
  (make-surface apparence rolling-resistance grip)
  surface?
  (apparence surface-apparence)
  (rolling-resistance surface-rr)
  (grip surface-grip))
             
(define surf:asphalt
  (make-surface "#2e2d2d" .01 .9))

(define surf:grass
  (make-surface "#33ff0a" .05 .5))

(define surf:sand
  (make-surface "#f3ff0a" .1 .3))
