(define-module (levels level-1)
  #:use-module (rallisp track)
  #:use-module (rallisp segment)
  #:use-module (rallisp surface)
  #:use-module (math vector)
  #:export (make-level-1))

(define (make-level-1)
  (let ([track (make-track surf:grass)]
        [road (make-segment surf:asphalt 70)]
        [sand (make-segment surf:sand 200)]
        [glass (make-segment surf:glass 200)]
        [alternative (make-segment surf:compact-sand 40)])
    (add-segment-point! road (vec2 -300 0))
    (add-segment-point! road (vec2 0 0))
    (add-segment-point! road (vec2 400 100))
    (add-segment-point! road (vec2 600 200))
    (add-segment-point! road (vec2 800 400))
    (add-segment-point! road (vec2 1000 500))
    (add-segment-point! road (vec2 1500 500))
    (add-segment-point! road (vec2 1700 400))
    (add-segment-point! road (vec2 2100 -100))
    (add-segment-point! road (vec2 2500 -200))
    (add-segment-point! road (vec2 2700 0))
    (add-segment-point! road (vec2 2800 -400))
    (add-segment-point! road (vec2 2800 -800))
    (add-segment-point! road (vec2 2700 -1100))
    (add-segment-point! road (vec2 2300 -1500))
    (add-segment-point! road (vec2 2000 -2500))
    (add-segment-point! road (vec2 2000 -3500))
    (add-track-segment! track road)

    (add-segment-point! glass (vec2 2000 -400))
    (add-segment-point! glass (vec2 2500 -800))
    (add-segment-point! glass (vec2 2000 -800))
    (add-track-segment! track glass)

    (add-segment-point! sand (vec2 800 100))
    (add-segment-point! sand (vec2 1000 100))
    (add-segment-point! sand (vec2 1500 200))
    (add-track-segment! track sand)

    (add-segment-point! alternative (vec2 400 100))
    (add-segment-point! alternative (vec2 400 100))
    (add-segment-point! alternative (vec2 800 -200))
    (add-segment-point! alternative (vec2 1500 -200))
    (add-segment-point! alternative (vec2 2100 -100))
    (add-track-segment! track alternative)

    track))
                        

               
