#lang typed/racket
(require racket/file)
(require racket/set)

(define input-file "./day_3.input.txt")

(: string->set (-> String (Setof Char)))
(define (string->set str)
  (list->set (string->list str)))

(: split-pockets (-> String (Pair (Setof Char) (Setof Char))))
(define (split-pockets input)
  (let ([half-point (round (/ (string-length input) 2))])
    (cons (string->set (substring input 0 half-point))
          (string->set (substring input half-point)))))
        

(: common-letter (-> (Pair (Setof Char) (Setof Char)) (Option Char)))
(define (common-letter pockets)
  ;; (print (set->list (set-intersect (car pockets) (cdr pockets))))
  (match (set->list (set-intersect (car pockets) (cdr pockets)))
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
    (foldl sum-priorities 0 (map common-letter (map split-pockets input)))))
              
(day-3)
