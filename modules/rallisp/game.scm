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

(define-module (rallisp game)
  #:use-module (srfi srfi-9)
  #:use-module (rallisp object)
  #:use-module (rallisp car)
  #:use-module (rallisp track)
  #:use-module (rallisp surface)
  #:use-module (dom document)
  #:use-module (math vector)
  #:export (<game>
            make-game
            game?
            game-state
            game-viewport
            set-game-viewport!
            set-game-state!
            register-object!
            register-car!
            game-draw
            game-track
            set-game-track!
            game-collides?
            game-collisions
            game-update!))

(define-record-type <game>
  (%make-game state viewport objects cars track)
  game?
  (state game-state set-game-state!)
  (viewport game-viewport set-game-viewport!)
  (objects game-objects set-game-objects!)
  (cars game-cars set-game-cars!)
  (track game-track set-game-track!))

(define (nil? x)
  (eq? x '()))

(define (make-game)
  (%make-game 'prompt (vec2 0 0) '() '() (make-track surf:grass)))

(define (register-object! game object)
  (set-game-objects! game
                     (cons object (game-objects game))))

(define (register-car! game c)
  (set-game-cars! game
                 (cons c (game-cars game))))

(define (game-draw game context)
  (let ([viewport (game-viewport game)])
    (track-draw (game-track game) context viewport)
    (let lp ([objects (game-objects game)])
      (unless (eq? '() objects)
        (object-draw (car objects) context viewport)
        (lp (cdr objects))))))

(define (game-update! game dt)
  (let lp ([cars (game-cars game)])
    (unless (eq? '() cars)
      (car-update! (car cars) dt (game-track game))
      (lp (cdr cars))))
  (let lp ([objects (game-objects game)])
    (unless (eq? '() objects)
      (object-update! (car objects) dt)
      (lp (cdr objects)))))

(define (game-collides? game object)
  "Return #f if the object collides with other objects. Not optimized at all, horrible"
  (let lp ([objects (game-objects game)])
    (if (nil? objects)
        #f
        (if (object-collides? object (car objects))
            #t
            (lp (cdr objects))))))

(define (game-collisions game)
  (let lp ([objects (game-objects game)]
           [collisions '()])
    (if (nil? objects)
        collisions
        (let ([o (car objects)]
              [rest (cdr objects)])
          (if (game-collides? game o)
              (lp rest
                (cons o collisions))
              (lp rest collisions))))))
          
