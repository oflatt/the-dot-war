#lang racket
(require rackunit 2htdp/universe 2htdp/image mzlib/string
         (only-in racket/gui/base get-display-size))
(require "circle-hit.rkt")
(get-display-size)
;VARIABLES
;---------------------------------------------------------------
(define HEIGHT 745)
(define WIDTH 1260)
(define SPEED 1)
(define RADIUS 10)
(define BULLET-RADIUS 2)
(define BULLET-SPEED 3)

;STRUCTURE
;---------------------------------------------------------------
;; wall-picture : list-of-wall -> image
(struct with-bullets (room bullets)#:prefab)
(struct room (dot walls)#:prefab)
(struct dot (posx posy keys)#:prefab)

;DEFINITIONS
;---------------------------------------------------------------
(define (start-game)
  (big-bang
   (with-bullets (room (dot 0 0 empty) (list (list 40 0 40 200)(list 80 0 80 200))) empty)
   (on-tick bullet-arrow .01)
   (on-key bullet-add-key)
   (on-release bullet-key-off)
   (on-draw bullet-picture)))

(define (bullet-add-key b key)
  (with-bullets
   (room-add-key (with-bullets-room b) key)
   (cond
     [(empty? key)
      (with-bullets-bullets b)]
     [else
      (cond
        [(empty? (with-bullets-bullets b))
         (new-bullet b key)]
        [else
         (with-bullets-bullets b)])])))

(define (new-bullet b key) 
  (cond
    [(equal? key "up")
     (list (dot-posx (room-dot (with-bullets-room b))) (dot-posy (room-dot (with-bullets-room b))) 'u)]
    [(equal? key "left")
     (list (dot-posx (room-dot (with-bullets-room b))) (dot-posy (room-dot (with-bullets-room b))) 'l)]
    [(equal? key "right")
     (list (dot-posx (room-dot (with-bullets-room b))) (dot-posy (room-dot (with-bullets-room b))) 'r)]
    [(equal? key "down")
     (list (dot-posx (room-dot (with-bullets-room b))) (dot-posy (room-dot (with-bullets-room b))) 'd)]
    [else
     (with-bullets-bullets b)]))

(define (room-add-key r key)
  (room
   (add-key (room-dot r) key)
   (room-walls r)))

(define (add-key d key)
  (dot
   (dot-posx d)
   (dot-posy d)
   (cons key (dot-keys d))))

(define (bullet-arrow b)
  (define f (with-bullets-bullets b))
  (with-bullets 
   (room-arrows (with-bullets-room b))
   (cond
     [(empty? f)
      empty]
     [(equal? (third f) 'l)
      (cond
        [(touches-any-segment-bullet (list (- (first f) BULLET-SPEED) (second f)) (room-walls (with-bullets-room b)))
         empty]
        [else
         (list (- (first f) BULLET-SPEED) (second f) (third f))])]
     [(equal? (third f) 'r)
      (cond
        [(touches-any-segment-bullet (list (+ (first f) BULLET-SPEED) (second f)) (room-walls (with-bullets-room b)))
         empty]
        [else
         (list (+ (first f) BULLET-SPEED) (second f) (third f))])]
     [(equal? (third f) 'u)
      (cond
        [(touches-any-segment-bullet (list (first f) (- (second f) BULLET-SPEED)) (room-walls (with-bullets-room b)))
         empty]
        [else
         (list (first f) (- (second f) BULLET-SPEED) (third f))])]
     [(equal? (third f) 'd)
      (cond
        [(touches-any-segment-bullet (list (first f) (+ (second f) BULLET-SPEED)) (room-walls (with-bullets-room b)))
         empty]
        [else
         (list (first f) (+ (second f) BULLET-SPEED) (third f))])]
     [else
      empty])))

(define (touches-any-segment-bullet b w)
  (cond
    [(empty? w)
     #f]
    [else
     (define p (first w))
     (or
      (>= (+ (+ (first b) RADIUS) BULLET-RADIUS) WIDTH)
      (<= (+ (+ (first b) RADIUS) BULLET-RADIUS) 0)
      (>= (+ (+ (second b) RADIUS) BULLET-RADIUS) HEIGHT)
      (<= (+ (+ (second b) RADIUS) BULLET-RADIUS) 0)
      (my-circle-touches-line-segment?
       (+ (first b) RADIUS) (+ (second b) RADIUS) BULLET-RADIUS
       (first p) (second p) (third p) (fourth p))
      (touches-any-segment-bullet b (rest w)))]))

;(define (touches-any-segment-y-bullet b w)
;(cond
;[(empty? w)
;#f]
;[else
;(define p (first w))
;(or
;(my-circle-touches-line-segment?
;(+ (dot-posx d) RADIUS) (+ (+ (second (pressed (dot-keys d))) (dot-posy d)) RADIUS) RADIUS
;(first p) (second p) (third p) (fourth p))
;(touches-any-segment-y d (rest w)))]))

(define (room-arrows w)
  (room
   (arrows (room-dot w) (touches-any-segment-x (room-dot w) (room-walls w)) (touches-any-segment-y (room-dot w) (room-walls w)))
   (room-walls w)))

(define (arrows d tx ty)
  (dot
   (cond
     [tx
      (dot-posx d)]
     [(> (+ (+ (first (pressed (dot-keys d))) (dot-posx d)) (* 2 RADIUS)) WIDTH)
      (- WIDTH (* 2 RADIUS))]
     [(< (+ (first (pressed (dot-keys d))) (dot-posx d)) 0)
      0]
     [else
      (+ (first (pressed (dot-keys d))) (dot-posx d))])
   (cond
     [ty
      (dot-posy d)]
     [(> (+ (+ (second (pressed (dot-keys d))) (dot-posy d))(* 2 RADIUS)) HEIGHT)
      (- HEIGHT (* 2 RADIUS))]
     [(< (+ (second (pressed (dot-keys d))) (dot-posy d)) 0)
      0]
     [else
      (+ (second (pressed (dot-keys d))) (dot-posy d))])
   (dot-keys d)))

(define (touches-regular-line? y1 y2 cy cr)
  (cond
    [(<= cy y1)
     (<= (- y1 cy) cr)]
    [else
     (<= (- cy y1) cr)]))

(define (between a b c)
  (or
   (<= b a c)
   (>= b a c)))

(define (my-circle-touches-line-segment? cx cy cr
                                         x1 y1
                                         x2 y2)
  (cond
    [(and (equal? y1 y2) (between cx x1 x2))
     (touches-regular-line? y1 y2 cy cr)]
    [(and (equal? x1 x2) (between cy y1 y2))
     (touches-regular-line? x1 x2 cx cr)]
    [else
     (circle-touches-line-segment? cx cy cr x1 y1 x2 y2)]))

(check-equal? (my-circle-touches-line-segment? 511 511 10 480 500 600 500) #f)
(check-equal? (my-circle-touches-line-segment? 509 509 10 480 500 600 500) #t)
(check-equal? (my-circle-touches-line-segment? 520 520 10 420 500 600 500) #f)
(check-equal? (my-circle-touches-line-segment? 512 511 10 480 500 600 500) #f)
(check-equal? (my-circle-touches-line-segment? 512 511 10 480 500 600 500) #f)

(define (touches-any-segment-x d w)
  (cond
    [(empty? w)
     #f]
    [else
     (define p (first w))
     (or
      (my-circle-touches-line-segment?
       (+ (+ (first (pressed (dot-keys d))) (dot-posx d)) RADIUS) (+ (dot-posy d) RADIUS) RADIUS
       (first p) (second p) (third p) (fourth p))
      (touches-any-segment-x d (rest w)))]))

(define (touches-any-segment-y d w)
  (cond
    [(empty? w)
     #f]
    [else
     (define p (first w))
     (or
      (my-circle-touches-line-segment?
       (+ (dot-posx d) RADIUS) (+ (+ (second (pressed (dot-keys d))) (dot-posy d)) RADIUS) RADIUS
       (first p) (second p) (third p) (fourth p))
      (touches-any-segment-y d (rest w)))]))

(define (pressed l)
  (list
   (cond
     [(and (find-key "a" l)
           (find-key "d" l))
      0]
     [(find-key "a" l)
      (- SPEED)]
     [(find-key "d" l)
      SPEED]
     [else 0])
   (cond
     [(and (find-key "w" l)
           (find-key "s" l))
      0]
     [(find-key "w" l)
      (- SPEED)]
     [(find-key "s" l)
      SPEED]
     [else 0])))

(define nothing (rectangle 0 0 'solid "white"))

(define (find-key key l)
  (cond
    [(empty? l)
     false]
    [(equal? (first l) key)
     true]
    [else
     (find-key key (rest l))]))

(check-equal? (find-key "bird" (list "bird" "apple")) true)
(check-equal? (find-key "blue" (list "right" "left")) false)

(define (bullet-picture d)
  (cond
    [(empty? (with-bullets-bullets d))
     (picture (with-bullets-room d))]
    [else
     (overlay/xy
      (circle BULLET-RADIUS "solid" "blue")
      (- (+ (first (with-bullets-bullets d)) RADIUS)) (- (+ (second (with-bullets-bullets d)) RADIUS))
      (picture (with-bullets-room d)))]))

(define (picture d)
  (overlay/xy
   (circle RADIUS "solid" "blue")
   (- (dot-posx (room-dot d))) (-  (dot-posy (room-dot d)))
   (wall-picture (room-walls d))))

(define (wall-picture l)
  (cond
    [(empty? l)
     (empty-scene WIDTH HEIGHT)]
    [else
     (define w (first l))
     (add-line (wall-picture (rest l))
               (first w) (second w) (third w) (fourth w)
               "black")]))

(define (bullet-key-off d key)
  (with-bullets
   (key-off (with-bullets-room d) key)
   (with-bullets-bullets d)))

(define (key-off d key)
  (room
   (dot
    (dot-posx (room-dot d))
    (dot-posy (room-dot d))
    (take-away key (dot-keys (room-dot d))))
   (room-walls d)))

(define (take-away key l)
  (cond
    [(empty? l) empty]
    [(equal? (first l) key)
     (take-away key (rest l))]
    [else
     (cons (first l) (take-away key (rest l)))]))

(check-equal? (take-away "left" (list "left" "up" "right" "left" "right" "up")) (list "up" "right" "right" "up"))

(start-game)