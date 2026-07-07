#lang racket


(define (newpos pos move)
  (modulo
   (if (char=? move #\<)
     (sub1 pos)
     (add1 pos))
   100))

(define (move-robot robmoves [segmoves (reverse robmoves)] [robpos 0] [segpos 0] [temp 0])
  (if (null? robmoves)
    temp
    (let
      ([newrobpos (newpos robpos (car robmoves))]
       [newsegpos (newpos segpos (car segmoves))])
      (move-robot
       (cdr robmoves)
       (cdr segmoves)
       newrobpos
       newsegpos
       (if (= newrobpos newsegpos)
         (add1 temp) 
         temp)))))

(displayln
 (move-robot
  (string->list
   (read-line
    (open-input-file "inputs/day02.txt")))))

