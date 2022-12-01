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

(struct ElfCalAccum ([elves : (Listof Integer)] [sum : Integer]))

(: input->elf-cals (-> (Listof String) (Listof Integer)))
(define (input->elf-cals input-list)
  (: calculate-elf-cals (-> String ElfCalAccum ElfCalAccum))
  (define (calculate-elf-cals input accum)
    (match* (input accum)
      [("" (ElfCalAccum elves sum)) (ElfCalAccum (cons sum elves) 0)]
      [(x (ElfCalAccum elves sum)) (ElfCalAccum elves (+ sum (string->integer x)))]))
  (let ([result (foldl calculate-elf-cals (ElfCalAccum '() 0) input-list)])
    (cons (ElfCalAccum-sum result) (ElfCalAccum-elves result))))

(: day-1-pt2 (-> Integer))
(define (day-1-pt2)
  (foldl + 0 (take (sort (input->elf-cals (open-file input-file)) >) 3)))

(day-1-pt2)
