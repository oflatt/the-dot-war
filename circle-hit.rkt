#lang racket

(provide circle-touches-line-segment?)

; circle-touches-line-segment?
;  Given a circle centered at (cx, cy) of radius cr
;  and a line segment from (x1, y1) to (x2, y2), does
;  the circle touch the line segment?
(define (circle-touches-line-segment? cx cy cr
                                      x1 y1
                                      x2 y2)
  ;; Length of the line segment:
  (define line-len (segment-length x1 y1 x2 y2))
  ;; The angle that the line segment makes relative to
  ;; the X-axis:
  (define line-a (segment-angle x1 y1 x2 y2))
  ;; Length of a new line segment from one end of
  ;; the given line segment to the middle of the circle:
  (define to-circle-len (segment-length x1 y1 cx cy))
  ;; The angle that the new line segment makes:
  (define to-circle-a (segment-angle x1 y1 cx cy))
  ;; The difference between those angles, because
  ;; we want to make a triangle with them:
  (define da (abs (- line-a to-circle-a)))
  ;; Make a right triangle using the new line segment
  ;; as the hypotenuse. The "far" side goes from the
  ;; circle center and is perpendicular to the original
  ;; line segment. The "overlap" side overlaps the line
  ;; segment, but may meet the "far" side (at a right angle)
  ;; beyond one of the ends of the line segment. We
  ;; multiply the sin and cosine of the triangle's angle
  ;; by the hypotenuse's length to get the length of each side:
  (define far-side-len (* to-circle-len (sin da)))
  (define overlap-side-len (* to-circle-len (cos da)))
  ;; Like `to-circle-len`, but for the other end of
  ;; the line segment:
  (define to-circle-other-len (segment-length x2 y2 cx cy))
  ;; The circle hits when...
  (or
   ;; ... it's within the circle's radius of the line segment's
   ;; first point, or ...
   (<= to-circle-len cr)
   ;; ... it's within the circle's radius of the line segment's
   ;; second point, or ...   
   (<= to-circle-other-len cr)
   ;; ... we can make a triangle, and the "far" side is shorter
   ;; than the circle's radius, and the "overlap" side doesn't stick
   ;; out of either end of the line segment.
   (and (<= 0 far-side-len cr)
        (<= 0 overlap-side-len line-len))))

; segment-length
;  Returns the length of the line segment
;  from (x1, y1) to (x2, y2) using the 
;  Pythagorean theorem
(define (segment-length x1 y1 x2 y2)
  (define dx (- x2 x1))
  (define dy (- y2 y1))
  (sqrt (+ (* dx dx) (* dy dy))))

; segment-angle
;  Returns the angle of the line segment
;  from (x1, y1) to (x2, y2) relative to the 
;  X-axis
(define (segment-angle x1 y1 x2 y2)
  (define dx (- x2 x1))
  (define dy (- y2 y1))
  (if (and (zero? dy) (zero? dx))
      0
      (atan dy dx)))

;; Some tests
(module+ test
  (require rackunit)
  ;; centered on line:
  (check-equal? (circle-touches-line-segment? 10 0 5
                                              0 0
                                              20 0)
                #t)
  ;; touching line middle:
  (check-equal? (circle-touches-line-segment? 10 -3 5
                                              0 0
                                              20 0)
                #t)
  ;; above line:
  (check-equal? (circle-touches-line-segment? 10 -6 5
                                              0 0
                                              20 0)
                #f)
  ;; below line:
  (check-equal? (circle-touches-line-segment? 10 6 5
                                              0 0
                                              20 0)
                #f)
  ;; too far to right:
  (check-equal? (circle-touches-line-segment? 24 4 5
                                              0 0
                                              20 0)
                #f)
  ;; too far to left:
  (check-equal? (circle-touches-line-segment? -4 4 5
                                              0 0
                                              20 0)
                #f)
  ;; hit right end:
  (check-equal? (circle-touches-line-segment? 24 0 5
                                              0 0
                                              20 0)
                #t)
  
  ;; hit left end:
  (check-equal? (circle-touches-line-segment? -4 0 5
                                              0 0
                                              20 0)
                #t)
  ;; diagonal, hit:
  (check-equal? (circle-touches-line-segment? 10 10 5
                                              0 0
                                              20 20)
                #t)
  ;; diagonal, miss:
  (check-equal? (circle-touches-line-segment? 6 6 5
                                              0 0
                                              20 20)
                #t))
;; There are a lot more cases we could test!
  
;; When you run this file directly in DrRacket, you'll
;; get a test window. When you `require` this file from
;; another one, the test window will not be shown.     
(module+ main
  (require racket/gui)
  
  (define cx 200)
  (define cy 200)
  (define cr 50)
  (define x1 100)
  (define y1 200)
  (define x2 300)
  (define y2 200)
  
  (define f (new frame%
                 [label "Check"]
                 [width 400]
                 [height 400]))
  (define c (new (class canvas%
                   (inherit get-dc
                            refresh)
                   (super-new [parent f])
                   
                   (define/override (on-paint)
                     (define hit?
                       (circle-touches-line-segment? cx cy cr
                                                     x1 y1
                                                     x2 y2))
                     (define dc (get-dc))
                     (send dc set-brush (make-brush #:color (if hit? "red" "green")))
                     (send dc set-pen (make-pen #:style 'transparent))
                     (send dc draw-ellipse (- cx cr) (- cy cr) (* 2 cr) (* 2 cr))
                     (send dc set-pen (make-pen #:width 1))
                     (send dc set-brush (make-brush #:style 'transparent))
                     (send dc draw-line x1 y1 x2 y2)
                     (send dc draw-text 
                           "Click to move point, right (or control) click to move other"
                           0 0 #t))
                   
                   (define/override (on-event e)
                     (define x (send e get-x))
                     (define y (send e get-y))
                     (cond
                       [(send e button-down? 'left)
                        (set! x1 x)
                        (set! y1 y)]
                       [(send e button-down? 'right)
                        (set! x2 x)
                        (set! y2 y)]
                       [else
                        (set! cx x)
                        (set! cy y)])
                     (refresh)))))
  (send f show #t))
