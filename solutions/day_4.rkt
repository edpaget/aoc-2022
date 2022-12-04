#lang typed/racket
(require racket/file)
(require racket/string)
(require/typed racket/list
  [inclusive-range (-> Integer Integer Integer (Listof Integer))])

(define input-file "./day_4.input.txt")

(: string->integer (-> String Integer))
(define (string->integer in)
  (let ([n (string->number in)])
    (if n (assert n exact-integer?) 0)))

(: range-from-input (-> String (Listof Integer)))
(define (range-from-input input)
  (let ([range-start-end (string-split input "-")])
    (inclusive-range (string->integer (first range-start-end))
                     (string->integer (second range-start-end))
                     1)))

(: clean-up-pair-from-input (-> String (List (Listof Integer) (Listof Integer))))
(define (clean-up-pair-from-input input)
  (let ([group (string-split input ",")])
    (list (range-from-input (first group))
          (range-from-input (second group)))))

(: pair-exactly-contain (-> (List (Listof Integer) (Listof Integer)) Boolean))
(define (pair-exactly-contain pair)
  (let ([intersect (apply set-intersect pair)])
    (or (= (length intersect) (length (first pair)))
        (= (length intersect) (length (second pair))))))

(: pair-overlap (-> (List (Listof Integer) (Listof Integer)) Boolean))
(define (pair-overlap pair)
  (let ([intersect (apply set-intersect pair)])
    (> (length intersect) 0)))

(: day-4 (-> Integer))
(define (day-4)
  (length (filter pair-exactly-contain (map clean-up-pair-from-input (file->lines input-file)))))

(day-4)

(: day-4-pt2 (-> Integer))
(define (day-4-pt2)
  (length (filter pair-overlap (map clean-up-pair-from-input (file->lines input-file)))))

(day-4-pt2)
