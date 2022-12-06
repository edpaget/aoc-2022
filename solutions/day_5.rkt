#lang typed/racket
(require racket/file)
(require racket/string)

(define input-file "day_5.input.txt")

(struct RawStacksAndMoves ([stacks : (Listof String)] [moves : (Listof String)]))
(define-type Stack (Vectorof (Listof String)))
(define-type Move (List Integer Integer Integer))

(: string->integer (-> String Integer))
(define (string->integer in)
  (let ([n (string->number in)])
    (if n (assert n exact-integer?) 0)))

(: stacks-from-input (-> (Listof String) RawStacksAndMoves))
(define (stacks-from-input lines)
  (: stacks-recur (-> (Listof String) (Listof String) RawStacksAndMoves))
  (define (stacks-recur stacks lines)
    (if (or (equal? (first lines) "") (empty? lines))
        (RawStacksAndMoves stacks (rest lines))
        (stacks-recur (cons (first lines) stacks) (rest lines))))
  (stacks-recur '() lines))

(: number-of-stacks (-> String Integer))
(define (number-of-stacks index-list)
  (: max-integer (-> Char Integer Integer))
  (define (max-integer str current-max)
    (max (string->integer (string str)) current-max))
  (foldl max-integer 0 (string->list index-list)))

;; assumes the stack is 'upside-down' from the input
(: new-stack (-> (Listof String) Stack))
(define (new-stack stack-state)
  (let* ([stack-count (number-of-stacks (first stack-state))]
         [vec : (Vectorof (Listof String)) (make-vector stack-count '())])
    (: stack-fold (-> String Stack Stack))
    (define (stack-fold level stack)
      (for ([idx (in-range stack-count)])
        (let* ([stack-idx (+ 1 (* 4 idx))]
               [item (substring level stack-idx (+ stack-idx 1))])
          (when (not (equal? item " "))
            (vector-set! vec idx (cons item (vector-ref vec idx))))))
      vec)
    (foldl stack-fold vec (rest stack-state))))

(: string->move (-> String Move))
(define (string->move str)
  (let ([m (filter exact-positive-integer? (map string->integer (string-split str " ")))])
    (list (first m) (- (second m) 1) (- (third m) 1))))

(: apply-move-pt1 (-> Move Stack Stack))
(define (apply-move-pt1 move stack)
  (vector-set! stack (third move) (append (reverse (take (vector-ref stack (second move)) (first move)))
                                          (vector-ref stack (third move))))
  (vector-set! stack (second move) (drop (vector-ref stack (second move)) (first move)))
  stack)

(: apply-move-pt2 (-> Move Stack Stack))
(define (apply-move-pt2 move stack)
  (vector-set! stack (third move) (append (take (vector-ref stack (second move)) (first move))
                                          (vector-ref stack (third move))))
  (vector-set! stack (second move) (drop (vector-ref stack (second move)) (first move)))
  stack)

(: day-5 (-> (-> Move Stack Stack) String))
(define (day-5 apply-move)
  (let* ([data (stacks-from-input (file->lines input-file))]
         [stack (new-stack (RawStacksAndMoves-stacks data))]
         [moves (map string->move (RawStacksAndMoves-moves data))])
    (foldl (lambda ([stack : (Listof String)] [out : String])
             (match stack
               [(list a _ ...) (string-append out a)]
               [(list a) (string-append out a)]
               [(list) out]))
           ""
           (vector->list (foldl apply-move stack moves)))))

(day-5 apply-move-pt1)
(day-5 apply-move-pt2)
