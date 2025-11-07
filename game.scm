;;; Lizzie Crowdagger, 2025.
;;;
;;; Based on the hoot game jam template:
;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
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

(use-modules (dom canvas)
             (dom document)
             (dom element)
             (dom event)
             (dom image)
             (dom media)
             (dom window)
             (hoot ffi)
             (hoot hashtables)
             (ice-9 match)
             (math)
             (math rect)
             (math vector)
             (srfi srfi-9)
             (rallisp object))

(define obj (make-object (vec2 100 100) 16.0 (vec2 0 0) 0 (make-image "assets/images/car.png")))
(log (format #f "foo: ~a\n" (object-x obj)))

;; Data types
(define-record-type <brick-type>
  (make-brick-type image points)
  brick-type?
  (image brick-type-image)
  (points brick-type-points))

(define-record-type <brick>
  (make-brick type hitbox)
  brick?
  (type brick-type)
  (hitbox brick-hitbox)
  (broken? brick-broken? set-brick-broken!))

(define-record-type <ball>
  (make-ball velocity hitbox)
  ball?
  (velocity ball-velocity)
  (hitbox ball-hitbox))

(define-record-type <paddle>
  (make-paddle velocity hitbox)
  paddle?
  (velocity paddle-velocity)
  (hitbox paddle-hitbox))

(define-record-type <level>
  (make-level state bricks ball paddle score move-left? move-right?)
  level?
  (state level-state set-level-state!) ; play, win, lose
  (bricks level-bricks)
  (ball level-ball)
  (paddle level-paddle)
  (score level-score set-level-score!)
  (move-left? level-move-left? set-level-move-left!)
  (move-right? level-move-right? set-level-move-right!))

;; Assets
(define image:paddle       (make-image "assets/images/paddle.png"))
(define image:ball         (make-image "assets/images/ball.png"))
(define image:brick-red    (make-image "assets/images/brick-red.png"))
(define image:brick-green  (make-image "assets/images/brick-green.png"))
(define image:brick-blue   (make-image "assets/images/brick-blue.png"))
(define audio:brick        (make-audio "assets/sounds/brick.wav"))
(define audio:paddle       (make-audio "assets/sounds/paddle.wav"))

;; Game data
(define game-width    640.0)
(define game-height   480.0)
(define brick-width   64.0)
(define brick-height  32.0)
(define ball-width    22.0)
(define ball-height   22.0)
(define paddle-width  104.0)
(define paddle-height 24.0)
(define paddle-speed  6.0)
(define brick:red     (make-brick-type image:brick-red 10))
(define brick:green   (make-brick-type image:brick-green 20))
(define brick:blue    (make-brick-type image:brick-blue 30))

(define (make-brick* type x y)
  (make-brick type (make-rect x y brick-width brick-height)))

(define (make-brick-grid types)
  (let* ((h (vector-length types))
         (w (vector-length (vector-ref types 0)))
         (offset-x (/ (- game-width (* w brick-width)) 2.0))
         (offset-y 48.0)
         (bricks (make-vector (* w h))))
    (do ((y 0 (+ y 1)))
        ((= y h))
      (let ((row (vector-ref types y)))
        (do ((x 0 (+ x 1)))
            ((= x w))
          (vector-set! bricks (+ (* y w) x)
                       (make-brick* (vector-ref row x)
                                    (+ offset-x (* x brick-width))
                                    (+ offset-y (* y brick-height)))))))
    bricks))

(define (make-level-1)
  (make-level 'play
              (make-brick-grid
               (vector
                (vector brick:red brick:green brick:blue brick:red brick:green brick:blue brick:red brick:green)
                (vector brick:green brick:blue brick:red brick:green brick:blue brick:red brick:green brick:blue)
                (vector brick:blue brick:red brick:green brick:blue brick:red brick:green brick:blue brick:red)
                (vector brick:red brick:green brick:blue brick:red brick:green brick:blue brick:red brick:green)
                (vector brick:green brick:blue brick:red brick:green brick:blue brick:red brick:green brick:blue)
                (vector brick:blue brick:red brick:green brick:blue brick:red brick:green brick:blue brick:red)))
              (make-ball (vec2 1.0 3.0)
                         (make-rect (/ game-width 2.0) (/ game-height 2.0)
                                    ball-width ball-height))
              (make-paddle (vec2 0.0 0.0)
                           (make-rect (- (/ game-width 2.0)
                                         (/ paddle-width 2.0))
                                      (- game-height paddle-height 8.0)
                                      paddle-width paddle-height))
              0 #f #f))

;; Game state
(define *level* (make-level-1))

(define (level-clear? level)
  (let ((bricks (level-bricks level)))
    (let loop ((i 0))
      (if (< i (vector-length bricks))
          (if (brick-broken? (vector-ref bricks i))
              (loop (+ i 1))
              #f)
          #t))))

(define (win! level)
  (set-level-state! level 'win))

(define (lose! level)
  (set-level-state! level 'lose))

(define (update-paddle-velocity! level)
  (let ((speed (* paddle-speed
                  (+ (if (level-move-left? level) -1.0 0.0)
                     (if (level-move-right? level) 1.0 0.0)))))
    (set-vec2-x! (paddle-velocity (level-paddle level)) speed)))

(define (speed-up-ball! ball)
  (let* ((v (ball-velocity ball))
         (speed (+ (vec2-magnitude v) (* (random) 0.1)))
         ;; Also change its angle slightly.  Not the proper Breakout
         ;; behavior but I don't want to write the code for that. :)
         (dir (+ (atan (vec2-y v) (vec2-x v))
                 (- (* (random) 0.5) 0.25))))
    (set-vec2-x! v (* (cos dir) speed))
    (set-vec2-y! v (* (sin dir) speed))))

(define (reflect-ball! ball x? y?)
  (let ((v (ball-velocity ball)))
    (when x? (set-vec2-x! v (- (vec2-x v))))
    (when y? (set-vec2-y! v (- (vec2-y v))))))

(define (collide-ball! ball hitbox)
  (let ((b-hitbox (ball-hitbox ball)))
    (and (rect-intersects? b-hitbox hitbox)
         (let ((overlap (rect-clip b-hitbox hitbox)))
           ;; Resolve collision by adjusting the ball's position the
           ;; minimum amount along the X or Y axis.
           (if (< (rect-width overlap) (rect-height overlap))
               (begin
                 (reflect-ball! ball #t #f)
                 (if (= (rect-x b-hitbox) (rect-x overlap))
                     (set-rect-x! b-hitbox (+ (rect-x b-hitbox) (rect-width overlap)))
                     (set-rect-x! b-hitbox (- (rect-x b-hitbox) (rect-width overlap)))))
               (begin
                 (reflect-ball! ball #f #t)
                 (if (= (rect-y b-hitbox) (rect-y overlap))
                     (set-rect-y! b-hitbox (+ (rect-y b-hitbox) (rect-height overlap)))
                     (set-rect-y! b-hitbox (- (rect-y b-hitbox) (rect-height overlap))))))))))

(define dt (/ 1000.0 60.0)) ; aim for updating at 60Hz
(define (update)
  (match 'play
    ('play
     (object-update! obj)
     (set-object-rotation! obj (+ (object-rotation obj) 0.01)))
    (_ #t))
  (timeout update-callback dt))
(define update-callback (procedure->external update))

;; Rendering
(define number->string*
  (let ((cache (make-eq-hashtable))) ; assuming fixnums only
    (lambda (x)
      (or (hashtable-ref cache x)
          (let ((str (number->string x)))
            (hashtable-set! cache x str)
            str)))))

(define (draw prev-time)
  ;; Clear canvas
  (clear-rect context 0.0 0.0 game-width game-height)
  ;; Draw background
  (set-fill-color! context "#006600")
  (fill-rect context 0.0 0.0 game-width game-height)

  (draw-line context 3 "red" (object-x obj) (object-y obj) pos-x pos-y)
    
  (object-draw obj context)
    
  ;; Print score
  (set-fill-color! context "#ffffff")
  (set-font! context "bold 24px monospace")
  (set-text-align! context "left")
  (fill-text context "PLOP!" 100 0)
  (request-animation-frame draw-callback))
(define draw-callback (procedure->external draw))

;; Input
(define key:left "ArrowLeft")
(define key:right "ArrowRight")
(define key:confirm "Enter")

(define pos-x 0)
(define pos-y 0)

(define (on-move event)
  (let* ([offset-x (bounding-client-x canvas)]
         [offset-y (bounding-client-y canvas)]
         [x (- (mouse-x event) offset-x)]
         [y (- (mouse-y event) offset-y)])
    (set! pos-x x)
    (set! pos-y y)))
  
(define (on-click event)
  (log (format #f "~a ~a, ~a\n" (mouse-button event) (mouse-x event) (mouse-y event)))
  (log (format #f "~a, ~a\n" (bounding-client-x canvas) (bounding-client-y canvas)))
  (let* ([offset-x (bounding-client-x canvas)]
         [offset-y (bounding-client-y canvas)]
         [x (- (mouse-x event) offset-x)]
         [y (- (mouse-y event) offset-y)])
    (log (format #f "~a ~a\n" x y))))


(define (on-key-down event)
  (let ((key (keyboard-event-code event)))
    (match (level-state *level*)
      ('play
       (cond
        ((string=? key key:left)
         (set-level-move-left! *level* #t)
         (update-paddle-velocity! *level*))
        ((string=? key key:right)
         (set-level-move-right! *level* #t)
         (update-paddle-velocity! *level*))))
      ((or 'win 'lose)
       (when (string=? key key:confirm)
         (set! *level* (make-level-1)))))))

(define (on-key-up event)
  (let ((key (keyboard-event-code event)))
    (match (level-state *level*)
      ('play
       (cond
        ((string=? key key:left)
         (set-level-move-left! *level* #f)
         (update-paddle-velocity! *level*))
        ((string=? key key:right)
         (set-level-move-right! *level* #f)
         (update-paddle-velocity! *level*))))
      (_ #t))))

;; Canvas and event loop setup
(define canvas (get-element-by-id "canvas"))
(define context (get-context canvas "2d"))

(set-element-width! canvas (inexact->exact game-width))
(set-element-height! canvas (inexact->exact game-height))
(add-event-listener! (current-document) "keydown"
                     (procedure->external on-key-down))
(add-event-listener! (current-document) "keyup"
                     (procedure->external on-key-up))
(add-event-listener! canvas "click" (procedure->external on-click))
(add-event-listener! canvas "mousemove" (procedure->external on-move))
(request-animation-frame draw-callback)
(timeout update-callback dt)
