#lang racket


(define part-one-rules
  (lambda (str)
    (apply
     +
     (map
      (lambda (r) (if (regexp-match r str) 1 0))
      (list #rx"[a-z]" #rx"[A-Z]" #rx"[0-9]")))))

(define match-seven
  (lambda (str)
    (if (regexp-match #rx"7" str)
      (if (regexp-match #rx"[012345689]" str) 0 7)
      0)))

(define match-repeats
  (lambda (str)
    (let
      ([repeats (regexp-match* #px"(.)\\1\\1+" str)])
      (if (null? repeats)
        0
        (let
          ([l (apply max (map string-length repeats))])
          (* l l))))))

(define match-rgb
  (lambda (str)
    (if
      (ormap
       (lambda (r) (regexp-match r str))
       (list #rx"red" #rx"green" #rx"blue"))
      3 
      1)))

(define score
  (lambda (str)
    (*
     (+ (part-one-rules str) (match-seven str) (match-repeats str))
     (match-rgb str)
     (string-length str))))

(define read-input
  (lambda (in)
    (let ([line (read-line in)])
      (if (eof-object? line)
        '()
        (cons (list line (score line)) (read-input in))))))

(displayln
 (car
  (argmax
   second
   (call-with-input-file "inputs/day03.txt" read-input))))

