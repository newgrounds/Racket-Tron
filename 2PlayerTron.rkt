#lang class/1
(require class/universe)
(require 2htdp/image)

(define WIDTH 400)
(define HEIGHT 400)
(define TRON-SIZE 10)
(define EMPTY (empty-scene WIDTH HEIGHT))

;; A Location is a Complex
;; A Velocity is a Complex
;; A Block is a Location

;; A LoB is a [Listof Block]

;; A Tron-Data is either: 
;;  - (list LoB Velocity)
;;  - [Listof Tron-Data]

;; A Tron is a (new tron% LoB Color)

(define-interface server-tron<%>
  [;; A Client is an IWorld
   client
   ;; A [Listof Block]
   lob
   ;; A Color
   col
   ;; dead? : Tron -> Boolean
   ;; determine if this tron has collided with anything
   dead?
   ;; head-hit? : Tron -> Boolean
   ;; determine if this tron has run headfirst into 
   ;;   any part of itself or the given tron
   head-hit?
   ;; hit-wall? : -> Boolean
   ;; determine if this tron has collided with a wall
   hit-wall?
   ;; generate-data : -> Tron-Data
   ;; creates a tron-data from the class fields
   generate-data
   ;; step : Velocity -> Tron
   ;; updates the tron with its next location
   ;;   based on the given velocity
   step])

(define-class server-tron%
  (implements server-tron<%>)
  (fields client lob col)
  
  (define (dead? other)
    (or (hit-wall?) (head-hit? other)))
  
  (define (head-hit? other)
    (contains? (first (field lob)) (append (rest (field lob)) (send other lob))))
  
  (define (hit-wall?)
    (not (and (<= 0 (real-part (first (field lob)))  WIDTH)
              (<= 0 (imag-part (first (field lob)))  HEIGHT))))
  
  (define (generate-data)
    (list (field lob)
          (field col)))
  
  (define (step v)
    (new server-tron%
         (field client)
         (cons (+ v (first (field lob)))
               (field lob))
         (field col))))

(define-interface universe<%>
  [;; the first tron
   t1
   ;; the second tron
   t2
   ;; update-tron : IWorld Number -> Universe
   ;; Updates the correct tron object based on the input world
   update-tron
   ;; on-new : IWorld -> Bundle
   ;; handles connection of clients
   on-new
   ; game-over? : -> Boolean
   ; Determines if the game is over
   game-over?
   ; get-winner : -> String
   ; Determines who won and produces the appropriate text
   get-winner
   ; mail-to : Tron Tron-Data -> Mail
   ; Provides an easy way to create mail
   mail-to
   ;; mail-both : Tron-Data -> [Listof Mail]
   ;; creates a list of mail from tron-data
   mail-both
   ; game-on? : -> Boolean
   ; Determines whether both clients are connected
   game-on?
   ;; generate-data : -> Tron-Data
   ;; creates a tron-data from the tron objects
   generate-data
   ;; on-msg : IWorld Velocity -> [oneOf: Universe | Bundle]
   ;; handles messages received from clients
   on-msg])

;; A Tron is a (new server-tron% IWorld LoB Color)

;; contains? : Block LoB -> Boolean
;; determines if the block is the same as another in the list of blocks
(define (contains? needle haystack)
  (ormap (lambda(x)(equal? x needle)) haystack))



(define-class universe%
  (implements universe<%>)
  (fields t1 t2)
  
  (define (update-tron iw vel)
    (cond 
      [(equal? (send (field t1) client) iw) 
       (new universe% (send (field t1) step vel) (field t2))]
      [(equal? (send (field t2) client) iw)
       (new universe% (field t1) (send (field t2) step vel))]))
  
  (define (on-new iw)
    (cond 
      ;; Accept the first client as player 1
      [(false? (send (field t1) client))
       (make-bundle (new universe% 
                         (new server-tron% iw (list 50+200i) "blue")
                         (field t2))
                    (list (make-mail iw (list 'assign-p1 "You are P1"))) empty)]    
      ;; Accept the second client as player 2
      [(false? (send (field t2) client))
       (make-bundle (new universe%
                         (field t1)
                         (new server-tron% iw (list 350+200i) "red"))
                    empty empty)] 
      ;; Reject any additional clients
      [else (make-bundle this empty (list iw))]))
  
  (define (game-over?)
    (or (send (field t1) dead? (field t2))
        (send (field t2) dead? (field t1))))
  
  (define (get-winner)
    (cond
      [(and  (send (field t1) dead? (field t2))
             (send (field t2) dead? (field t1))) "Tie"]
      [(send (field t1) dead? (field t2)) "Player 2 Won"]
      [(send (field t2) dead? (field t1)) "Player 1 Won"]))
  
  (define (mail-to t data)
    (make-mail (send t client) data))
  
  (define (mail-both data)
    (list (mail-to (field t1) data)
          (mail-to (field t2) data)))
  
  (define (game-on?)
    (not (or (false? (send (field t1) client))
             (false? (send (field t2) client)))))
  
  (define (generate-data)
    (list 'game-on
          (list (send (field t1) generate-data)
                (send (field t2) generate-data))))
  
  (define (on-msg iw vel)
    ;; First, make sure both clients are connected before doing anything
    (cond [(not (game-on?)) (make-bundle this empty empty)]
          [(game-over?)
           (make-bundle this (mail-both (list 'game-over (get-winner))) empty)]
          [else
           (let ([nw (update-tron iw vel)]) ;; Only make the new world once.
             ;; Makes bundle when client two sends, not both, so that the data
             ;; only needs to be generated and sent every other request.
             (if (equal? iw (send (field t2) client))
                 (make-bundle nw (mail-both (send nw generate-data)) empty)
                 (make-bundle nw empty empty)))])))










(define-interface tron<%>
  [;; A LoB
   lob
   ;; A Color
   col
   ;; draw : Scene -> Scene
   ;; draws the tron on the given scene
   draw])

(define-class tron%
  (implements tron<%>)
  (fields lob col)

  
  (define (draw scn)
    (foldr (λ (blk scene)
             (place-image (square TRON-SIZE "solid" (field col))
                          (real-part blk)
                          (imag-part blk)
                          scene))
           scn
           (field lob))))

(define-interface world<%>
  [;; This tron
   me
   ;; The other tron
   other
   ;; whether or not this tron is the first tron
   p1?
   ;; A Velocity
   vel
   ;; The drawn scene
   drawn
   ;; data->tron : Tron-Data -> Tron
   ;; creates a tron object from the given tron-data
   data->tron
   ;; update-drawing : -> World
   update-drawing
   ;; game-over-text : String Scene  -> Scene
   ;; the scene to display when the game ends
   game-over-text
   ;; on-receive : Tron-Data -> World
   ;; responds to the server with a package
   on-receive
   ;; on-tick : -> Package
   ;; updates the server with the current state of the world
   on-tick
   ;; to-draw : -> Scene
   ;; draws the world to a scene
   to-draw
   ;; on-key : String -> World
   ;; updates the world on key events
   on-key])

(define-class world%
  (implements world<%>)
  (fields me other p1? vel drawn)
  
  (define (tick-rate) 1/5) 
  
  (define (register) "127.0.0.1")
  
  (define (data->tron td)
    (new tron% 
         (first td) ; The LoB
         (first (rest td)))) ; The Color
  
  (define (update-drawing)
    (new world% 
         (field me)
         (field other)
         (field p1?)
         (field vel)
         (place-image 
          (text (if (field p1?) "Player 1" "Player 2") 16 'black) 
          (/ WIDTH 2) 20
          (send (field me) draw (send (field other) draw EMPTY)))))
  
  ;; DOES THIS NEED TO TAKE IN THE SCENE IF IT'S ALREADY A FIELD IN THIS CLASS?
  (define (game-over-text txt scn)
    (let ([txt-img (text txt 20 'black)])
      (overlay 
       txt-img
       (rectangle (+ (image-width txt-img) 10) 40 'outline 'black)
       (rectangle (+ (image-width txt-img) 10) 40 'solid 'white)
       scn)))
  
  (define (on-receive gd)
    (let ([request (first gd)]
          [data   (second gd)])
      (cond [(equal? 'assign-p1 request)
             (new world% (field me) (field other) true 10+0i (field drawn))]
            [(equal? 'game-over request)
             (stop-with (new world%
                             (field me)
                             (field other)
                             (field p1?)
                             (field vel)
                             (game-over-text data (field drawn))))]
            [(equal? 'game-on request)
             (new world%
                  (if (field p1?) (data->tron (first data))  (data->tron (second data)))
                  (if (field p1?) (data->tron (second data)) (data->tron (first data)))
                  (field p1?)
                  (field vel)
                  (field drawn))])))
  
  (define (on-tick)
    (make-package (send this update-drawing) (field vel)))
  
  (define (to-draw)
    (field drawn))
  
  (define (on-key k)
    (let ([nv (cond [(string=? k "left") -10+0i]
                    [(string=? k "right") 10+0i]
                    [(string=? k "up") 0-10i]
                    [(string=? k "down") 0+10i]
                    [else (field vel)])])
      (if (= (* -1 nv) (field vel))
        this
        (new world% 
             (field me)
             (field other)
             (field p1?)
             nv
             (field drawn))))))



(launch-many-worlds ;; make multiple worlds
 (universe (new universe% 
               (new server-tron% false empty 'black) 
               (new server-tron% false empty 'black)))
 (big-bang (new world% (new tron% empty "blue") (new tron% empty "red") false  -10+0i EMPTY))
 (big-bang (new world% (new tron% empty "blue") (new tron% empty "red") false  -10+0i EMPTY)))