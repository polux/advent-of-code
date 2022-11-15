#lang curly-fn at-exp racket

(require advent-of-code)
(require threading)
(require relation/composition)
(require relation/type)
(require racket/match)

(define input-str
  (fetch-aoc-input (find-session) 2021 12 #:cache #t))

(define example-str @string-append{
 fs-end
 he-DX
 fs-he
 start-DX
 pj-DX
 end-zg
 zg-sl
 zg-pj
 pj-he
 RW-he
 fs-DX
 pj-RW
 zg-RW
 start-pj
 he-WI
 zg-he
 pj-fs
 start-RW})

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
  (define (big? str) (char-upper-case? (string-ref str 0)))
  (define (count seen v)
    (cond
      [(set-member? seen v) 0]
      [(equal? v "end") 1]
      [else
       (define new-seen (if (big? v) seen (set-add seen v)))
       (for/sum ([n (hash-ref input v)])
         (count new-seen n))]))
  (count (set) "start"))

(~> input-str
    parse
    to-graph
    solve
    time)