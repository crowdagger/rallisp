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
            surface-name
            surface-apparence
            surface-rr
            surface-grip
            surf:asphalt
            surf:grass
            surf:sand
            surf:compact-sand))

(define-record-type <surface>
  (make-surface name apparence rolling-resistance grip)
  surface?
  (name surface-name)
  (apparence surface-apparence)
  (rolling-resistance surface-rr)
  (grip surface-grip))
             
(define surf:asphalt
  (make-surface "asphalt" "#2e2d2d" .05 .9))

(define surf:grass
  (make-surface "grass" "#33ff0a" .15 .5))

(define surf:sand
  (make-surface "sand" "#f3ff0a" .3 .3))

(define surf:compact-sand
  (make-surface "compact-sand" "#994d0b" .1 .8))
