#lang racket


(define read-input
  (lambda (in [lst '()])
    (let ([line (read-line in)])
      (if (eof-object? line)
        lst
        (let
          ([new-lst
            (cond
              [(string=? line "o-|"  ) (cons #true  lst)]
              [(string=? line "  |-o") (cons #true  lst)]
              [(string=? line "  |"  ) (cons #false lst)]
              [else lst])])
          (read-input in new-lst))))))

(length
 (filter
  identity
  (drop
   (call-with-input-file "inputs/day04.txt" read-input)
   400)))

