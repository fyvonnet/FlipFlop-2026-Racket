#lang racket


(define segments (make-vector 100 0))

(define (move-robot moves [pos 0])
  (unless (null? moves)
    (let*
      ([newpos
        (modulo
         (if (char=? (car moves) #\<)
           (sub1 pos)
           (add1 pos))
         100)])
      (vector-set! segments newpos (add1 (vector-ref segments newpos)))
      (move-robot (cdr moves) newpos))))

(define (combine lst [pos 1])
  (if (null? lst)
    '()
    (cons
     (cons (car lst) pos)
     (combine
      (cdr lst)
      (add1 pos)))))

(move-robot
 (string->list
  (read-line
   (open-input-file "inputs/day02.txt"))))

(displayln
 (match-let
   ([(cons a b)
     (car
      (sort
       (combine (vector->list segments))
       (lambda (x y)
         (let ([d (- (car y) (car x))])
           (cond
             [(positive? d) #false]
             [(negative? d) #true]
             [#true (< (cdr x) (cdr y))])))))])
   (* a b)))
