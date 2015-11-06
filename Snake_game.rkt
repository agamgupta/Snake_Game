;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Snake_game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Developed By: AGAM GUPTA

(require "snake-lib.rkt")
; Data Definitions: 
; a game is
; (make-game snake food nat)

; (define-struct game (snake food ticks))

; a direction is either
; - 'up
; - 'down
; - 'left
; - 'right

; a snake is
; (make-snake direction body)
; (define-struct snake (heading segments))

; a body is either
; - (cons posn empty)
; - (cons posn body)

; x-coordinates increase from 1 to board-length (inclusive) toward
; the right
; y-coordinates increase from 1 to board-length (inclusive) toward
; the top
; the default value for board-length is 50.

; a food is either
; - empty
; - (cons posn food)

; Test games ->

(define test-game1 
  (make-game (make-snake 'up (list (make-posn 3 3)))
                        (list (make-posn 1 2)) 1))
(define test-game2 
  (make-game (make-snake 'down (list (make-posn 7 7)(make-posn 7 8)))
                          empty 1))
(define test-game3 
  (make-game (make-snake 'left (list (make-posn 1 6)(make-posn 1 7)))
                        (list (make-posn 2 3)) 1))

(define test-game4 
  (make-game (make-snake 'left (list (make-posn 0 18)(make-posn 0 19)))
                        (list (make-posn 2 3)) 1))
(define test-game5 
  (make-game (make-snake 'right (list (make-posn 23 20)(make-posn 22 20)
                                      (make-posn 21 20)))
                        (list (make-posn 24 20)) 2))
; Test snakes -> 
(define test-snake1
  (make-snake 'up (list (make-posn 3 3))))
(define test-snake2
  (make-snake 'down (list (make-posn 7 7)(make-posn 7 8))))
(define test-snake3
  (make-snake 'right (list (make-posn 12 6))))

; add-food : game posn -> game
; takes in a game and a position and adds a food at that position
(define (add-food g p)
  (make-game (game-snake g) (cons p (game-food g)) (game-ticks g)))

(check-expect (add-food (make-game (make-snake 'up (list (make-posn 3 3)))
                        (list (make-posn 0 0)) 1) (make-posn 5 4)) 
                        (make-game (make-snake 'up (list (make-posn 3 3)))
                        (cons (make-posn 5 4)(list (make-posn 0 0))) 1))  
(check-expect (add-food test-game2 (make-posn 5 5)) 
              (make-game test-snake2 (list (make-posn 5 5)) 1))             

; change-direction: game direction -> game
; takes in a game and a direction and changes the direction of the snake as 
; inputted
(define (change-direction g dir) 
  (make-game (make-snake dir (snake-segments (game-snake g)))
                             (game-food g) 
                             (game-ticks g)))

(check-expect (change-direction test-game1 'right)
              (make-game (make-snake 'right (list (make-posn 3 3)))
                         (list (make-posn 1 2)) 1))

(check-expect (change-direction test-game2 'up)
              (make-game (make-snake 'up (list (make-posn 7 7)(make-posn 7 8)))
                          empty 1))

; game-score: game -> nat
; takes in a game and returns a score
(define (game-score g)
  (- (length(snake-segments(game-snake g))) 1))

(check-expect (game-score test-game1) 0)
(check-expect (game-score test-game2) 1)
  
; Helper Function
; count list -> number
(define (count l)
  (cond 
    [(empty? l)     0]
    [else        (+ 1 (count (rest l)))]))

; game-over? : game->boolean
; Takes in a game and returns a boolean -if it is over or not-
; based on a given condition.
(define (game-over? g)
  (local [(define fir (snake-segments (game-snake g)))]
    (cond  
      [(or (= (posn-x (first fir)) 0)
           (= (posn-x (first fir)) (+ 1 board-length))
           (= (posn-y (first fir)) 0)
           (= (posn-y (first fir)) (+ 1 board-length)))             true]
     [(member? (first fir) (rest fir))                              true]
     [else                                                       false])))

(check-expect (game-over? test-game1) false)
(check-expect (game-over? test-game4) true)
  
; advance-game : game -> game
; Takes in a game and returns the game with the food eaten by the snake 
; if we move the snake to the right position or simply move the snake on
; the screen.
  (define (advance-game g)
    (local [(define dir (snake-heading (game-snake g))) 
            (define fir (first(snake-segments (game-snake g))))
            (define futurehead
              (cond
                [(symbol=? dir 'up)        (make-posn (posn-x fir)
                                                      (+ 1 (posn-y fir)))]
                [(symbol=? dir 'down)      (make-posn (posn-x fir)
                                                      (- (posn-y fir) 1))]
                [(symbol=? dir 'right)     (make-posn (+ 1 (posn-x fir)) 
                                                      (posn-y fir))]
                [(symbol=? dir 'left)      (make-posn (- (posn-x fir) 1) 
                                                      (posn-y fir))]))]
    (cond
      [(member? futurehead (game-food g))   
                                         
                           (make-game  
                            (cond
                              [(symbol=? dir 'up) (make-snake dir 
                                                     (cons (make-posn 
                                                            (posn-x fir) 
                                                            (+ 1 (posn-y fir)))
                                                     (snake-segments
                                                             (game-snake g))))]
                                                        
                              [(symbol=? dir 'down) (make-snake 
                                                      dir 
                                                     (cons (make-posn
                                                            (posn-x fir) 
                                                            (- (posn-y fir) 1))
                                                     (snake-segments 
                                                             (game-snake g))))]
                              [(symbol=? dir 'right) (make-snake
                                                      dir
                                                     (cons (make-posn
                                                            (+ 1 (posn-x fir)) 
                                                            (posn-y fir))
                                                     (snake-segments 
                                                       (game-snake g))))]
                              [(symbol=? dir 'left) (make-snake 
                                                     dir
                                                     (cons (make-posn  
                                                            (- (posn-x fir) 1) 
                                                            (posn-y fir))
                                                     (snake-segments 
                                                        (game-snake g))))])
                           
                           (remove futurehead (game-food g)) 
                           (+ 1 (game-ticks g)))]                    
        
       
      [else                (make-game  
                            (cond
                             [(symbol=? dir 'up)  (make-snake 
                                                    dir
                                                    (cons (make-posn 
                                                           (posn-x fir) 
                                                           (+ 1 (posn-y fir)))
                                                       (remove-last
                                                        (snake-segments 
                                                           (game-snake g)))))]
                             [(symbol=? dir 'down) (make-snake 
                                                    dir
                                                    (cons (make-posn
                                                           (posn-x fir) 
                                                           (- (posn-y fir) 1))
                                                      (remove-last
                                                       (snake-segments
                                                        (game-snake g)))))]
                             [(symbol=? dir 'right) (make-snake dir
                                                     (cons (make-posn
                                                            (+ 1 (posn-x fir)) 
                                                            (posn-y fir))
                                                      (remove-last
                                                       (snake-segments
                                                        (game-snake g)))))]
                             [(symbol=? dir 'left) (make-snake dir
                                                      (cons (make-posn
                                                             (- (posn-x fir) 1) 
                                                             (posn-y fir))
                                                        (remove-last
                                                         (snake-segments
                                                          (game-snake g)))))])
                              
                           (game-food g) 
                           (+ 1 (game-ticks g)))])))
  
  (check-expect (advance-game test-game1) (make-game (make-snake 
                                                      'up
                                                     (list (make-posn 3 4)))
                        (list (make-posn 1 2)) 2))
  (check-expect (advance-game test-game2) (make-game(make-snake
                                                     'down
                                                       (list (make-posn 7 6)
                                                             (make-posn 7 7)))
                         empty 2))
  (check-expect (advance-game test-game5) 
  (make-game (make-snake 'right (list (make-posn 24 20)(make-posn 23 20)
                                      (make-posn 22 20)(make-posn 21 20)))
                       empty 3))
  
  
; Helper Function: 
; remove-last: list->list
; takes in a list and removes the last element of the list 

(define (remove-last l)
  (cond
    [(empty? l)             empty]
    [(empty? (rest l))      empty]
    [else                  (cons (first l)(remove-last (rest l)))]))        
              
 (check-expect (remove-last (list 1 2 3 4 5)) (list 1 2 3 4)) 
 
; Creating a test game and then enjoying it :) !
 
(define game-start (make-game (make-snake 'up (list (make-posn 1 3)))
                              (cons 
                               (make-posn (+ 1 (random 50)) 
                                          (+ 1 (random 50)))
                               (list (make-posn 
                                      (+ 1 (random 50)) 
                                      (+ 1 (random 50)))
                                     (make-posn
                                      (+ 1 (random 50)) 
                                      (+ 1 (random 50)))))
                                2))
(play-game game-start
           advance-game
           add-food
           change-direction
           game-score
           game-over?)