#lang typed/racket
(require racket/file)
(require racket/string)

(define input-file "./day_2.input.txt")

(define-type GameAction (U 'rock 'paper 'scissors))
(define-type Game (Pair GameAction GameAction))

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
            ["X" 'rock]
            ["Y" 'paper]
            ["Z" 'scissors]))))

(: score-wins (-> GameAction GameAction Integer))
(define (score-wins opp me)
  (match* (opp me)
    [('rock 'rock) 3]
    [('rock 'paper) 6]
    [('rock 'scissors) 0]
    [('paper 'rock) 0]
    [('paper 'paper) 3]
    [('paper 'scissors) 6]
    [('scissors 'rock) 6]
    [('scissors 'paper) 0]
    [('scissors 'scissors) 3]))

(: choice-score (-> GameAction Integer))
(define (choice-score action)
  (match action
    ['rock 1]
    ['paper 2]
    ['scissors 3]))

(: score-game (-> Game Integer))
(define (score-game game)
  (+ (score-wins (car game) (cdr game))
     (choice-score (cdr game))))

(: day-2 (-> Integer))
(define (day-2)
  (let ([input (file->lines input-file)])
    (foldl + 0 (map score-game (map parse-game input)))))

(day-2)
