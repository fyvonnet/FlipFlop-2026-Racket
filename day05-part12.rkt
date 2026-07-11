#lang racket

(define arrows
  (make-hash
   '([#\^ . ( 0 -1)]
     [#\> . ( 1  0)]
     [#\v . ( 0  1)]
     [#\< . (-1  0)])))

(define width 0)
(define city-orig '())
(define city '())

(define read-input
  (lambda (in)
    (let ([line (read-line in)])
      (if (eof-object? line)
        '()
        (cons line (read-input in))))))

(define street-ref
  (lambda (coord)
    (string-ref city (+ (first coord) (* (second coord) width)))))

(define street-set!
  (lambda (coord chr)
    (string-set! city (+ (first coord) (* (second coord) width)) chr)))

(define drive
  (lambda ([coord '(0 0)] [cnt 0] [lst '()])
    (match (street-ref coord)
      [#\@ (values cnt lst)]
      [arrow
       (street-set! coord #\@)
       (drive
        (for/list
          ([c coord] [m (hash-ref arrows arrow)])
          (+ c m))
        (add1 cnt)
        (cons coord lst))])))

(define not-border 
  (lambda (coord)
    (match coord
      [(list col row)
       (and
        (positive? col)
        (positive? row)
        (< col (sub1 width))
        (< row (sub1 width)))])))

(define change-arrow
  (lambda (coord)
    (lambda (arrow)
      (set! city (string-copy city-orig))
      (street-set! coord arrow)
      (match/values (drive) [(cnt _) cnt]))))

(define change-street
  (lambda (coord)
    (apply
     max
     (map
      (change-arrow coord)
      (remove (street-ref coord) '(#\^ #\> #\v #\<))))))

(let
  ([input (call-with-input-file "inputs/day05.txt" read-input)])
  (set! width (string-length (car input)))
  (set! city-orig (apply string-append input))
  (set! city (string-copy city-orig))
  (match/values (drive)
    [(cnt route)
     (displayln cnt)
     (let
       ([valid (filter not-border route)])
       (set! city (string-copy city-orig))
       (apply max (map change-street valid)))]))

