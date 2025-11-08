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

;;; A track is a list of segment, plus default surface

(define-module (rallisp track)
  #:use-module (srfi srfi-9)
  #:use-module (rallisp surface)
  #:use-module (rallisp segment)
  #:export (<track>
            track?
            make-track
            track-segments
            track-draw
            track-default-surface
            add-track-segment!))

(define-record-type <track>
  (%make-track default-surface segments)
  track?
  (default-surface track-default-surface)
  (segments track-segments set-track-segments!))

(define (make-track default-surface)
  (%make-track default-surface '()))

(define (add-track-segment! track segment)
  (let ([segments (track-segments track)])
    (set-track-segments! track (cons segment segments))))

(define (track-draw track context)
  (let lp ([segments (track-segments track)])
    (unless (eq? '() segments)
      (segment-draw (car segments) context)
      (lp (cdr segments)))))
