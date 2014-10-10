#lang racket
(require rackunit 2htdp/universe 2htdp/image mzlib/string
         (only-in racket/gui/base get-display-size))
(require "circle-hit.rkt")

;;MAP is a list like this structure: (struct map (walls starting-b starting-r)#:prefab)
;; walls is a list of walls, and a wall is a list of x1 and y1 and x2 and y2
(define MAP (list (list (list 40 0 40 200)(list 80 0 80 200)) (list 0 0) (list 100 100)))
(define HEIGHT 745)
(define WIDTH 1260)
(define SPEED 1)
(define RADIUS 10)
(define BULLET-RADIUS 2)
(define BULLET-SPEED 5)
(define SEND-MESSAGE 3)

;; list of games and false or world
(struct server (games waiting clock)#:prefab)
;;a message out is a list of with-bullets, score, a map, and the color of that dot
(struct game (with-bullets score client1 client2 map)#:prefab)
;; blue dot first in list of dots
;; Bullets is a list of lists. The first list is the x y and direction of the blue bullet.
(struct with-bullets (dots bullets)#:prefab)
;; the coordinates of the dots and bullets are the top left of the dot or bullet.
(struct dot (posx posy keys)#:prefab)

(define (start-universe)
  (universe
   (server empty #f 0)
   [on-new new-client]
   [state "OliverFlatt"]
   [on-msg on-msg-key]
   [on-tick server-tick 0.01]
   [port 50001]))

;; SERVER TICK ###################################################################################################################################################################
(define (server-tick b)
  (define results (games-tick (server-games b)))
  (cond
    [(equal? (server-clock b) SEND-MESSAGE)
     (make-bundle
      (server
       results
       (server-waiting b)
       0)
      (tick-messages results (server-games b))
      empty)]
    [else
     (server
      results
      (server-waiting b)
      (+ (server-clock b) 1))]))
      

(define (tick-messages r l)
  (cond
    [(empty? r)
     empty]
    [else
     (define old-g (first l))
     (define new-g (first r))
     (append
      (one-game-message old-g new-g)
      (tick-messages (rest r) (rest l)))]))

(define (one-game-message o n)
  (cond
    [(equal? o n)
     empty]
    [else
     (list
      (make-mail (game-client1 n)
                 (list (game-with-bullets n)
                       (game-score n)
                       (game-map n)))
      (make-mail (game-client2 n) 
                 (list (game-with-bullets n)
                       (game-score n)
                       (game-map n))))]))
     
(module+ test
  (check-equal?
   (tick-messages
    empty)
   empty))

(module+ test
  (define iworld4 iworld1)
  (check-equal?
   (tick-messages
    (list
     (game
      (with-bullets
       (list
        (dot 50 50 empty)
        (dot 60 60 empty))
       empty)
      (list 0 0)
      iworld1
      iworld2
      MAP)
     (game
      (with-bullets
       (list
        (dot 20 20 empty)
        (dot 40 40 empty))
       empty)
      (list 0 0)
      iworld3
      iworld4
      MAP)))
   (list
    (make-mail iworld1
               (list
                (with-bullets
                 (list
                  (dot 50 50 empty)
                  (dot 60 60 empty))
                 empty)
                (list 0 0)
                MAP))
    (make-mail iworld2
               (list
                (with-bullets
                 (list
                  (dot 50 50 empty)
                  (dot 60 60 empty))
                 empty)
                (list 0 0)
                MAP))
    (make-mail iworld3
               (list
                (with-bullets
                 (list
                  (dot 20 20 empty)
                  (dot 40 40 empty))
                 empty)
                (list 0 0)
                MAP))
    (make-mail iworld4
               (list
                (with-bullets
                 (list
                  (dot 20 20 empty)
                  (dot 40 40 empty))
                 empty)
                (list 0 0)
                MAP)))))

(define (games-tick l)
  (cond
    [(empty? l)
     empty]
    [else
     (cons
      (game-tick (first l))
      (games-tick (rest l)))]))

;; game-tick : game -> game
;; It moves things and checks for hitting walls and deaths
(define (game-tick g)
  (define (reset-game score)
    (game
     (with-bullets
      (list
       (dot (first (second MAP)) (second (second MAP)) empty)
       (dot (first (third MAP)) (second (third MAP)) empty))
      (list empty empty))
     score
     (game-client1 g)
     (game-client2 g)
     (game-map g)))
  (cond
    [(kills-other-dot-b g)
     (reset-game (list (+ 1 (first (game-score g))) (second (game-score g))))]
    [(kills-other-dot-r g)
     (reset-game (list (first (game-score g)) (+ 1 (second (game-score g)))))]
    [else
     (game
      (bullet-arrow g)
      (game-score g)
      (game-client1 g)
      (game-client2 g)
      (game-map g))]))
      

(module+ test
  (check-equal? 
   (game-tick
    (game
     (with-bullets
      (list
       (dot 10 10 (list "w"))
       (dot 0 0 empty))
      (list empty empty))
     '(0 0)
     iworld1
     iworld2
     MAP))
   (game
    (with-bullets
     (list
      (dot 10 (- 10 SPEED) (list "w"))
      (dot 0 0 empty))
     (list empty empty))
    '(0 0)
    iworld1
    iworld2
    MAP)))

(module+ test
  (define no-bullets (list empty empty))
  (check-equal? 
   (game-tick
    (game
     (with-bullets
      (list
       (dot 10 10 (list "a" "w"))
       (dot 0 0 empty))
      no-bullets)
     '(0 0)
     iworld1
     iworld2
     MAP))
   (game
    (with-bullets
     (list
      (dot (- 10 SPEED) (- 10 SPEED) (list "a" "w"))
      (dot 0 0 empty))
     no-bullets)
    '(0 0)
    iworld1
    iworld2
    MAP)))

(define (kills-other-dot g get-this get-other)
  (define b (with-bullets-bullets (game-with-bullets g)))
  (define d (get-this (with-bullets-dots (game-with-bullets g))))
  (cond
    [(empty? (get-other b))
     #f]
    [else
     (<=
      (sqrt
       (+
        (expt
         (- (+ (first (get-other b)) BULLET-RADIUS) (+ (dot-posx d) RADIUS)) 2)
        (expt
         (- (+ (second (get-other b)) BULLET-RADIUS) (+ (dot-posy d) RADIUS)) 2)))
      (+ BULLET-RADIUS RADIUS))]))

(module+ test
  (check-equal?
   (kills-other-dot
    (game
     (with-bullets
      (list (dot 50 50 empty)
            (dot 60 60 empty))
      (list empty empty))
     (list 0 0)
     iworld1
     iworld2
     MAP) second first)
   #f))

(module+ test
  (check-equal?
   (kills-other-dot
    (game
     (with-bullets
      (list (dot 50 50 empty)
            (dot 60 60 empty))
      (list (list 60 60 'r) empty))
     (list 0 0)
     iworld1
     iworld2
     MAP) second first)
   #t))

(module+ test
  (check-equal?
   (kills-other-dot
    (game
     (with-bullets
      (list (dot 50 50 empty)
            (dot 60 60 empty))
      (list empty (list 50 50 'r)))
     (list 0 0)
     iworld1
     iworld2
     MAP) first second)
   #t))

(define (kills-other-dot-b g)
  (kills-other-dot g second first))

(define (kills-other-dot-r g)
  (kills-other-dot g first second))

(define (touches-any-segment-bullet b w)
  (cond
    [(empty? w)
     (or
      (>= (+ (first b) BULLET-RADIUS) WIDTH)
      (<= (+ (first b) BULLET-RADIUS) 0)
      (>= (+ (second b) BULLET-RADIUS) HEIGHT)
      (<= (+ (second b) BULLET-RADIUS) 0))]
    [else
     (define p (first w))
     (or
      (my-circle-touches-line-segment?
       (+ (first b) BULLET-RADIUS) (+ (second b) BULLET-RADIUS) BULLET-RADIUS
       (first p) (second p) (third p) (fourth p))
      (touches-any-segment-bullet b (rest w)))]))

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

(define (move-bullet f b dx dy)
  (cond
    [(touches-any-segment-bullet (list (+ dx (first f)) (+ dy (second f))) (first (game-map b)))
     empty]
    [else
     (list (+ dx (first f)) (+ dy (second f)) (third f))]))

(define (one-bullet-arrow f b)
  (cond
    [(empty? f)
     empty]
    [(equal? (third f) 'l)
     (move-bullet f b (- BULLET-SPEED) 0)]
    [(equal? (third f) 'r)
     (move-bullet f b BULLET-SPEED 0)]
    [(equal? (third f) 'u)
     (move-bullet f b 0 (- BULLET-SPEED))]
    [(equal? (third f) 'd)
     (move-bullet f b 0 BULLET-SPEED)]
    [else
     empty]))

;; bullet-arrow : game -> with-bullets
;; It moves dots and the bullets based on current keys and directions
(define (bullet-arrow b)
  (define w (first (game-map b)))
  (with-bullets
   (dots-arrows (with-bullets-dots (game-with-bullets b)) w)
   (let ()
     (define f (first (with-bullets-bullets (game-with-bullets b))))
     (define f2 (second (with-bullets-bullets (game-with-bullets b))))
     (list
      (one-bullet-arrow f b)
      (one-bullet-arrow f2 b)))))

(define (dots-arrows d w)
  (list
   (arrows (first d) (touches-any-segment-x (first d) w) (touches-any-segment-y (first d) w))
   (arrows (second d) (touches-any-segment-x (second d) w) (touches-any-segment-y (second d) w))))

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

(module+ test
  (check-equal?
   (arrows (dot 10 10 (list "w")) #f #f)
   (dot 10 (- 10 SPEED) (list "w"))))

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

(define (find-key key l)
  (cond
    [(empty? l)
     false]
    [(equal? (first l) key)
     true]
    [else
     (find-key key (rest l))]))

;; NEW CLIENT ####################################################################################################################################################################

(define (new-client b w)
  (define the-map MAP)
  (define results 
    (cons
     (game
      (with-bullets 
       (list
        (dot (first (second the-map)) (second (second the-map)) empty)
        (dot (first (third the-map)) (second (third the-map)) empty)) (list empty empty))
      (list 0 0)
      (server-waiting b)
      w
      the-map)
     (server-games b)))
  (cond
    [(equal? (server-waiting b) #f)
     (make-bundle
      (server
       (server-games b)
       w
       (server-clock b))
      empty
      empty)]
    [else
     (make-bundle
      (server
       results
       #f
       (server-clock b))
      (list (make-mail w
                       (list
                        (game-with-bullets (first results))
                        (game-score (first results))
                        (game-map (first results))))
            (make-mail (server-waiting b)
                       (list
                        (game-with-bullets (first results))
                        (game-score (first results))
                        (game-map (first results)))))
      empty)]))

(define game-tester1
  (list
   (with-bullets
    (list
     (dot (first (second MAP)) (second (second MAP)) empty)
     (dot (first (third MAP)) (second (third MAP)) empty)) (list empty empty))
   (list 0 0)
   MAP))

(define game-tester2
  (game
   (with-bullets
    (list
     (dot (first (second MAP)) (second (second MAP)) empty)
     (dot (first (third MAP)) (second (third MAP)) empty)) (list empty empty))
   (list 0 0)
   iworld2
   iworld1
   MAP))

(check-equal? (new-client (server empty #f 4) iworld1) (make-bundle (server empty iworld1 4) empty empty))
(check-equal? (new-client (server empty iworld2 4) iworld1) 
              (make-bundle
               (server
                (list
                 game-tester2)
                #f
                4)
               (list
                (make-mail
                 iworld1
                 game-tester1)
                (make-mail
                 iworld2
                 game-tester1))
               empty))
;;ON MESSAGE #####################################################################################################################################################################

;; A message back is a bool telling if realease or not and a key

(define (on-msg-key b w m)
  (define results (new-list-of-games (server-games b) w m))
  (cond
    [(equal? (server-waiting b) #f)
     (make-bundle
      (server
       results
       (server-waiting b)
       (server-clock b))
      (message-key w results)
      empty)]
    [(equal? w (server-waiting b))
     b]
    [else
     (make-bundle
      (server
       results
       (server-waiting b)
       (server-clock b))
      (message-key w results)
      empty)]))

(define (message-key w l)
  (define g (wich-world? w l))
  (cond
    [g
     empty]
    [else
     (list
      (make-mail (game-client1 g) (first g))
      (make-mail (game-client2 g) (first g)))]))

(define (wich-world? w l)
  (cond
    [(empty? l)
     (list l empty)]
    [(or (equal? w (game-client1 (first l))) (equal? w (game-client2 (first l))))
     (first l)]
    [else
     (wich-world? (rest l))]))

(define (new-list-of-games l w m)
  (define f (first l))
  (cond
    [(equal? w (game-client1 f))
     (cons
      (game
       (bullet-add-key (game-with-bullets f) m 1)
       (game-score f)
       (game-client1 f)
       (game-client2 f)
       (game-map f))
      (rest l))]
    [(equal? w (game-client2 f))
     (cons
      (game
       (bullet-add-key (game-with-bullets f) m 2)
       (game-score f)
       (game-client1 f)
       (game-client2 f)
       (game-map f))
      (rest l))]
    [else
     (cons
      (new-list-of-games (rest l) w)
      f)]))

(module+ test
  (check-equal?
   (new-list-of-games
    (list
     (game
      (with-bullets
       (list
        (dot 50 50 (list "d" "g" "s"))
        (dot 80 80 (list "d" "w" "b")))
       (list empty empty))
      (list 0 0)
      iworld1
      iworld2
      MAP))
    iworld1
    (list #f "left"))
   (list
    (game
     (with-bullets
      (list
       (dot 50 50 (list "left" "d" "g" "s"))
       (dot 80 80 (list "d" "w" "b")))
      (list
       (list 58 58 'l) empty))
     (list 0 0)
     iworld1
     iworld2
     MAP))))

(define msg-release? first)

(define msg-key second)

(define (bullet-add-key b m n)
  (define key (msg-key m))
  (cond
    [(msg-release? m)
     (server-key-off b key n)]
    [else
     (cond
       [(equal? n 1)
        (with-bullets
         (list
          (dot-add-key (first (with-bullets-dots b)) key)
          (second (with-bullets-dots b)))
         (cond
           [(empty? (first (with-bullets-bullets b)))
            (list
             (new-bullet b key first)
             (second (with-bullets-bullets b)))]
           [else (with-bullets-bullets b)]))]
       [else
        (with-bullets
         (list
          (first (with-bullets-dots b))
          (dot-add-key (second (with-bullets-dots b)) key))
         (cond
           [(empty? (second (with-bullets-bullets b)))
            (list
             (first (with-bullets-bullets b))
             (new-bullet b key second))]
           [else (with-bullets-bullets b)]))])]))

(define (new-bullet b key wich-dot)
  (define (adj v) (- (+ v RADIUS) BULLET-RADIUS))
  (define (the-bullet sym) (list (adj (dot-posx (wich-dot (with-bullets-dots b))))
                                 (adj (dot-posy (wich-dot (with-bullets-dots b))))
                                 sym))
  (cond
    [(equal? key "up") (the-bullet 'u)]
    [(equal? key "down") (the-bullet 'd)]
    [(equal? key "left") (the-bullet 'l)]
    [(equal? key "right") (the-bullet 'r)]
    [else
     empty]))

(module+ test
  (check-equal?
   (bullet-add-key
    (with-bullets
     (list
      (dot 10 10 (list "w" "l"))
      (dot 60 60 (list "g" "s")))
     (list empty empty))
    (list #f "left")
    1)
   (with-bullets
    (list
     (dot 10 10 (list "left" "w" "l"))
     (dot 60 60 (list "g" "s")))
    (list
     (list 18 18 'l) empty))))
  
(define (server-key-off b key n)
  (cond
    [(equal? n 1)
     (with-bullets
      (list
       (key-off (first (with-bullets-dots b)) key)
       (second (with-bullets-dots b)))
      (with-bullets-bullets b))]
    [(equal? n 2)
     (with-bullets
      (list
       (first (with-bullets-dots b))
       (key-off (second (with-bullets-dots b)) key))
      (with-bullets-bullets b))]))

(define (key-off d key)
  (dot
   (dot-posx d)
   (dot-posy d)
   (take-away key (dot-keys d))))

(define (take-away key l)
  (cond
    [(empty? l) empty]
    [(equal? (first l) key)
     (take-away key (rest l))]
    [else
     (cons (first l) (take-away key (rest l)))]))

(define (dot-add-key d key)
  (dot
   (dot-posx d)
   (dot-posy d)
   (cons key (dot-keys d))))

(start-universe)