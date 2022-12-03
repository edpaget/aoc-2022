#lang typed/racket
(require racket/file)
(require racket/set)

(define input-file "./day_3.input.txt")

(: partition-every (All (a) (-> (Listof a) Integer (Listof (Listof a)))))
(define (partition-every input-list n)
  (: fold-fn (-> a (Listof (Listof a)) (Listof (Listof a))))
  (define (fold-fn in accum)
    (if (= (length (first accum)) n)
        (cons (list in) accum)
        (cons (cons in (first accum)) (rest accum))))
  (foldl fold-fn (list '()) input-list))

(: common-letter (-> (Listof (Listof Char)) (Option Char)))
(define (common-letter pockets)
  (assert pockets pair?)
  (match (apply set-intersect pockets)
    [(list a) a]
    [(list a ...) #f]
    [(list) #f]))

(: priority-score (-> Char Integer))
(define (priority-score chr)
  (if (char-lower-case? chr)
      (- (char->integer chr) 96)
      (- (char->integer chr) 38)))

(: sum-priorities (-> (Option Char) Integer Integer))
(define (sum-priorities chr accum)
  (+ accum (if chr (priority-score chr) 0)))

(: day-3 (-> Integer))
(define (day-3)
  (let ([input (file->lines input-file)])
    (foldl sum-priorities 0 (map common-letter (partition-every (map string->list input) 3)))))
              
(day-3)
