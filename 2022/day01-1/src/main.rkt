#lang curly-fn racket

(require advent-of-code)
(require threading)

(define str-input
  (fetch-aoc-input (find-session) 2022 1 #:cache #t))

(define (parse str)
  (~>> str
       (string-split _ "\n\n")
       (map (lambda~>> string-split
                       (map string->number)))))

(define (solve input)
  (~>> input
       (map #{apply + %})
       (apply max)))

(~>> str-input
     parse
     solve)