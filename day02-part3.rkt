#lang racket

; segments numbers indexed by position
(define segments (make-vector 100))
(for ([i (in-range 100)]) (vector-set! segments i i))

; temperatures indexed by segment number
(define temperatures (make-vector 100 0))

(define (makefunc move)
  (let ([inc (if (char=? move #\>) -1 1)])
    (lambda (x) (modulo (+ x inc) 100))))

(define (move-robot robmoves [segmoves (reverse robmoves)] [robpos 0])
  (unless (null? robmoves)
    (let ([newrobpos (modulo (+ (if (char=? (car robmoves) #\<) -1 1) robpos) 100)])
      (vector-map! (makefunc (car segmoves)) segments)
      (let ([segnum (vector-ref segments newrobpos)])
        (vector-set! temperatures segnum (add1 (vector-ref temperatures segnum)))
        (move-robot (cdr robmoves) (cdr segmoves) newrobpos)))))

(move-robot (string->list (read-line (open-input-file "inputs/day02.txt"))))
(apply * (argmax second (for/list ([i (in-naturals 1)] [t temperatures]) (list i t))))

