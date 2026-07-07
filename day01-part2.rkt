#lang racket

(define (read-input port)
  (let ([line (read-line port)])
    (if (eof-object? line)
      0
      (+
       (let ([n (- 60 (string->number line))])
         (if (positive? n) n (* 5 (- n))))
       (read-input port)))))

(displayln (call-with-input-file "inputs/day01.txt" read-input))

