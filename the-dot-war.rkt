#lang racket
(require rackunit 2htdp/universe 2htdp/image mzlib/string
         (only-in racket/gui/base get-display-size)
         "big-crunch.rkt")

;VARIABLES
;---------------------------------------------------------------
(define WAITING-IMAGE
  (overlay
   (text "waiting for match" 100 "indigo")
   (rectangle 100 100 'solid "white")))
(define HEIGHT 745)
(define WIDTH 1260)
(define SPEED 1)
(define RADIUS 10)
(define BULLET-RADIUS 2)
(define BULLET-SPEED 3)
(define ADDRESS "67.166.78.233")

;STRUCTURE
;---------------------------------------------------------------
(struct game (with-bullets score map)#:prefab)
;; blue dot first in list of dots
;; Bullets is a list of lists. The first list is the x y and direction of the blue bullet.
(struct with-bullets (dots bullets)#:prefab)
(struct dot (posx posy keys)#:prefab)

;DEFINITIONS
;---------------------------------------------------------------
;;state of waiting screen is false or the message

(define (start-game)
  (big-bang/big-crunch
   #f
   [name "waiting"]
   [on-receive connected]
   [stop-when (lambda (s) (not (not s)))]
   [on-draw waiting]
   [register ADDRESS]
   [port 50001]))

(define (connected s m)
  (game
   (first m)
   (second m)
   (third m)))

(define (waiting s)
  (overlay
   WAITING-IMAGE
   (empty-scene
    WIDTH
    HEIGHT)))

(define (new-game)
  (define init-state (start-game))
  (cond
    [init-state
     (big-bang
      init-state
      [name "The Dot War"]
      (on-key key-msg)
      (on-release key-off-msg)
      (on-receive msg-state)
      (on-draw bullet-picture)
      [register ADDRESS]
      [port 50001])]
    [else #f]))

(define (key-msg s k)
  (make-package
   s
   (list #f k)))

(define (key-off-msg s k)
  (make-package
   s
   (list #t k)))

(define (msg-state s m)
  (game
   (first m)
   (second m)
   (third m)))

(define nothing (rectangle 0 0 'solid "white"))

;;Picture ########################################################################################################################################################################

(define (bullet-picture d)
  (overlay/xy
   (text (number->string (first (game-score d))) 24 "blue")
   -20 (- (- HEIGHT 20))
   (overlay/xy
    (text (number->string (second (game-score d))) 24 "red")
    (- (- WIDTH 20)) (- (- HEIGHT 20))
    (add-one-bullet-picture
     (first (with-bullets-bullets (game-with-bullets d))) "blue"
     (add-one-bullet-picture
      (second (with-bullets-bullets (game-with-bullets d))) "red"
      (picture (with-bullets-dots (game-with-bullets d)) (game-score d) (game-map d)))))))

;; add-one-bullet-picture : bullet string pict -> pict
(define (add-one-bullet-picture bullet color p)
  (cond
    [(empty? bullet) p]
    [else
     (overlay/xy
      (circle BULLET-RADIUS "solid" color)
      (- (first bullet))
      (- (second bullet))
      p)]))

(define (picture d s w)
  (overlay/xy
   (circle RADIUS "solid" "red")
   (- (dot-posx (second d))) (-  (dot-posy (second d)))
   (overlay/xy
    (circle RADIUS "solid" "blue")
    (- (dot-posx (first d))) (-  (dot-posy (first d)))
    (wall-picture (first w)))))

(define (wall-picture l)
  (cond
    [(empty? l)
     (empty-scene WIDTH HEIGHT)]
    [else
     (define w (first l))
     (add-line (wall-picture (rest l))
               (first w) (second w) (third w) (fourth w)
               "black")]))

(new-game)
