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


;;; Segment of a track.

(define-module (rallisp segment)
  #:use-module (srfi srfi-9)
  #:use-module (math vector)
  #:use-module (dom canvas)
  #:use-module (dom document)
  #:use-module (rallisp surface)
  #:export (<segment>
            segment?
            make-segment
            segment-surface
            segment-start
            segment-end
            segment-radius
            segment-draw
            segment-distance
            in-segment?))

(define-record-type <segment>
  (make-segment surface start end radius)
  segment?
  (surface segment-surface)
  (start segment-start)
  (end segment-end)
  (radius segment-radius))

(define (segment-draw segment context viewport)
  "Draw the segment on the context given"
  (let* ([start (segment-start segment)]
         [end (segment-end segment)]
         [v-x (- (vec2-x viewport))]
         [v-y (- (vec2-y viewport))])
    (draw-line context
               (* 2 (segment-radius segment))
               (surface-apparence (segment-surface segment))
               (+ (vec2-x start) v-x)
               (+ (vec2-y start) v-y)
               (+ (vec2-x end) v-x)
               (+ v-y (vec2-y end)))))


(define (segment-distance segment point)
  "Distance between a segment and a point

Actually returns #f if the point can't be projected on the segment"
  (let* ([ab (vec2-sub (segment-end segment)
                      (segment-start segment))]
         [unit (vec2-normalize ab)]
         [length (vec2-magnitude ab)]
         [ap (vec2-sub point
                       (segment-start segment))]
         [dot (vec2-dot unit ap)]
         [wedge (vec2-wedge unit ap)])
    (if (and (positive? dot)
             (<= dot length))
        wedge
        #f)))

(define (in-segment? segment point)
  "Return #t if point is in segment, #f else"
  (let ([d (segment-distance segment point)]
        [r (segment-radius segment)])
    (if d
        (<= (abs d) r)
        #f)))
