#lang racket

(define (read-input port)
  (let ([line (read-line port)])
    (if (eof-object? line)
      '()
      (cons
       (string->number line)
       (read-input port)))))

(displayln
 (apply
  +
  (map
   (lambda (n) (- 60 n))
   (filter
    (lambda (n) (< n 60))
    (call-with-input-file
     "inputs/day01.txt"
     read-input)))))


