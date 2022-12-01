#lang typed/racket
(require racket/file)

(define input-file "./day_1.input.txt")

(: open-file (-> String (Listof String)))
(define (open-file file-path)
  (file->lines file-path))

(: string->integer (-> String Integer))
(define (string->integer str)
  (let ([result (string->number str)])
    (if result
        (assert result exact-integer?)
        0)))

(struct CalAccumulator ([max : Integer] [sum : Integer]))

(: find-max (-> String CalAccumulator CalAccumulator))
(define (find-max input accum)
  (match* (input accum)
    [("" (CalAccumulator max sum)) (if (> sum max) (CalAccumulator sum 0) (CalAccumulator max 0))]
    [(x (CalAccumulator max sum)) (CalAccumulator max (+ sum (string->integer x)))]))

(: day-1 (-> Integer))
(define (day-1)
  (let ([accum (foldl find-max (CalAccumulator 0 0) (open-file input-file))])
    (if (> (CalAccumulator-max accum) (CalAccumulator-sum accum))
        (CalAccumulator-max accum)
        (CalAccumulator-sum accum))))

(day-1)
