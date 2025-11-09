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
            segment-points
            add-segment-point!
            segment-radius
            segment-draw
            in-segment?))

(define-record-type <segment>
  (%make-segment surface points radius)
  segment?
  (surface segment-surface)
  (points segment-points set-segment-points!)
  (radius segment-radius))

(define (make-segment surface radius)
  (%make-segment surface '() radius))

(define (add-segment-point! segment point)
  (set-segment-points! segment (cons point (segment-points segment))))
  

(define (segment-draw segment ctx viewport)
  "Draw the segment on the context given"
  (set-line-width! ctx (* 2 (segment-radius segment)))
  (set-stroke-style! ctx (surface-apparence (segment-surface segment)))
  (begin-path! ctx)
  (let ([points (segment-points segment)]
        [v-x (- (vec2-x viewport))]
        [v-y (- (vec2-y viewport))])
    (unless (eq? '() points)
      (move-to! ctx
                (+ v-x (vec2-x (car points)))
                (+ v-y (vec2-y (car points))))
      (let lp ([points (cdr points)])
        (if (eq? points '())
            (stroke! ctx)
            (let ([point (car points)]
                  [rest (cdr points)])
              (line! ctx
                     (+ v-x (vec2-x point))
                     (+ v-y (vec2-y point)))
              (lp rest)))))))

(define (segment-distance a b point)
  "Distance between a segment and a point

Actually returns #f if the point can't be projected on the segment"
  (let* ([ab (vec2-sub b
                      a)]
         [unit (vec2-normalize ab)]
         [length (vec2-magnitude ab)]
         [ap (vec2-sub point
                       a)]
         [dot (vec2-dot unit ap)]
         [wedge (vec2-wedge unit ap)])
    (if (and (positive? dot)
             (<= dot length))
        wedge
        #f)))

(define (in-ab? a b r point)
  "Return #t if point is in segment, #f else"
  (let ([d (segment-distance a b point)])
    (if d
        (<= (abs d) r)
        #f)))

(define (in-segment? segment point)
  (let ([radius (segment-radius segment)]
        [points (segment-points segment)])
    (if (and (list? points)
             (> (length points) 2))
        (let lp ([a (car points)]
              [b (cadr points)]
              [rest (cddr points)])
          (if (in-ab? a b radius point)
              #t
              (if (eq? '() rest)
                  #f
                  (lp b
                      (car rest)
                      (cdr rest)))))
        #f)))
