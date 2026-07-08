#lang racket


(define regexs (map regexp (list "[a-z]" "[A-Z]" "[0-9]")))

(define score
  (lambda (str)
    (apply + (map (lambda (r) (if (regexp-match r str) 1 0)) regexs))))

(define read-input
  (lambda (in)
    (let ([line (read-line in)])
      (if (eof-object? line)
        '()
        (cons line (read-input in))))))

(displayln
 (car
  (argmax
   second
   (for/list
     ([str (call-with-input-file "inputs/day03.txt" read-input)])
     (list str (* (score str) (string-length str)))))))

