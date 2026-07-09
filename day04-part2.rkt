#lang racket


(define read-input
  (lambda (in [lst '()])
    (let ([line (read-line in)])
      (if (eof-object? line)
        lst
        (let
          ([new-lst
            (cond
              [(string=? line "o-|"  ) (cons 'left  lst)]
              [(string=? line "  |-o") (cons 'right lst)]
              [else lst])])
          (read-input in new-lst))))))

(define count-swaps
  (lambda (side leaves [count 0])
    (cond
      [(null? leaves) count]
      [(eq? (car leaves) side) (count-swaps side (cdr leaves) count)]
      [else (count-swaps (car leaves) (cdr leaves) (add1 count))])))

(let
  ([input
    (call-with-input-file "inputs/day04.txt" read-input)])
  (count-swaps (car input) (cdr input)))

