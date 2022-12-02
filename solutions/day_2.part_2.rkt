#lang typed/racket
(require racket/file)
(require racket/string)

(define input-file "./day_2.input.txt")

(define-type GameAction (U 'rock 'paper 'scissors))
(define-type GameResult (U 'win 'lose 'draw))
(define-type Game (Pair GameAction GameResult))

(: parse-game (-> String Game))
(define (parse-game input)
  (let* ([actions (string-split input " ")]
         [opp-action (first actions)]
         [me-action (second actions)])
    (cons (match opp-action
            ["A" 'rock]
            ["B" 'paper]
            ["C" 'scissors])
          (match me-action
            ["X" 'lose]
            ["Y" 'draw]
            ["Z" 'win]))))

(: score-play (-> GameAction GameResult Integer))
(define (score-play opp me)
  (match* (opp me)
    [('rock 'win) 2]
    [('rock 'lose) 3]
    [('rock 'draw) 1]
    [('paper 'win) 3]
    [('paper 'lose) 1]
    [('paper 'draw) 2]
    [('scissors 'win) 1]
    [('scissors 'lose) 2]
    [('scissors 'draw) 3]))

(: score-result (-> GameResult Integer))
(define (score-result action)
  (match action
    ['win 6]
    ['lose 0]
    ['draw 3]))

(: score-game (-> Game Integer))
(define (score-game game)
  (+ (score-play (car game) (cdr game))
     (score-result (cdr game))))

(: day-2 (-> Integer))
(define (day-2)
  (let ([input (file->lines input-file)])
    (foldl + 0 (map score-game (map parse-game input)))))

(day-2)
