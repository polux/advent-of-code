#lang racket

(require advent-of-code)
(require threading)
(require seq)
(require relation/composition)

(define str-input
  (fetch-aoc-input (find-session) 2021 1 #:cache #t))

(define (parse str)
  (~>> str
       string-split
       (map string->number)))

(define (solve input)
  (define (delta x y)
    (if (> y x) 1 0))
  (~>> (zip-with delta input (rest input))
       sum))

(define (pretty sol) sol)

(~>> str-input
     parse
     solve
     pretty)