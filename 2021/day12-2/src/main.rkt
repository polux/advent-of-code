#lang curly-fn at-exp racket

(require advent-of-code)
(require threading)
(require relation/composition)
(require relation/type)
(require racket/match)

(define input-str
  (fetch-aoc-input (find-session) 2021 12 #:cache #t))

(define example-str @string-append{
 start-A
 start-b
 A-c
 A-b
 b-d
 A-end
 b-end})

(define/contract (parse str)
  (-> string? (listof (list/c string? string?)))
  (define (split-line s) (string-split s "-"))
  (~>> str
       string-split
       (map split-line)))

(define (to-graph edges)
  (define res (make-hash))
  (for ([edge edges])
    (match-define (list s e) edge)
    (hash-update! res s (curry cons e) '())
    (hash-update! res e (curry cons s) '()))
  res)

(define (solve input)
  (define (small? str) (char-lower-case? (string-ref str 0)))
  (define (count joker-used seen v)
    (define v-seen (set-member? seen v))
    (cond
      [(equal? v "end") 1]
      [(and v-seen (or joker-used (equal? v "start"))) 0]
      [else
       (let ([new-joker-used (or joker-used v-seen)]
             [new-seen (if (small? v) (set-add seen v) seen)])
         (for/sum ([n (hash-ref input v)])
           (count new-joker-used new-seen n)))]))
  (count #f (set) "start"))

(define (pretty sol) sol)

(~> input-str
    parse
    to-graph
    solve
    pretty
    time)