#lang racket

(define (read-input port)
  (let ([line (read-line port)])
    (if (eof-object? line)
      '()
      (cons
       (string->number line)
       (read-input port)))))

(let-values
  ([(temp pref)
    (let
      ([input
        (call-with-input-file
         "inputs/day01.txt"
         read-input)])
      (split-at input (/ (length input) 2)))])
  (displayln
   (for/fold
     ([sum 0])
     ([t temp] [p pref])
     (let 
       ([n (- p t)])
       (+ sum (if (positive? n) n (* 5 (- n))))))))

