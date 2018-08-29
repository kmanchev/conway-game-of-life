#lang racket

;private methods
(define (append-tail elem lst)
  (foldr cons (list elem) lst))
  
(define (get-elem-at-index index ls)
  (cond ( (< index 1) 0)
        ( (> index (length ls) ) 0 )
        ( (= index 1) (car ls) )
        (else (get-elem-at-index (- index 1) (cdr ls) ) )
        )
  )

(define (in-range x y elem)
   (not (or (< elem x) (> elem y)))
  )

;game methods
(define (next-step rows)
  (next-step-helper rows 1 rows '())
  )

(define (next-step-helper work-rows index rows new)
  (if (null? work-rows) new
      (next-step-helper (cdr work-rows) (+ index 1) rows (append-tail (compute-row (car work-rows) index rows) new ))
      )
  )

(define (compute-row row row-index rows)
  (compute-row-helper row row-index rows 1 '())
  )

(define (compute-row-helper row row-index rows elem-index res)
  (if (null? row) res
      (compute-row-helper (cdr row) row-index rows (+ elem-index 1) (append-tail (compute-elem (car row) row-index rows elem-index) res) )
      )
  )

(define (compute-elem elem row-index rows elem-index)
  (cond ( (and (= elem 1) (in-range 2 3 (count-neighbours elem row-index rows elem-index) ) ) 1)
        ( (and (= elem 1) (< (count-neighbours elem row-index rows elem-index) 2) ) 0)
        ( (and (= elem 1) (> (count-neighbours elem row-index rows elem-index) 3) ) 0)
        ( (and (= elem 0) (= (count-neighbours elem row-index rows elem-index) 3) ) 1)
        (else elem)
        )
  )
	
(define (count-neighbours elem row-index rows elem-index) 
  (+ (get-matrix-elem-by-indexes rows (- row-index 1) elem-index )
     (get-matrix-elem-by-indexes rows (+ row-index 1) elem-index )
     (get-matrix-elem-by-indexes rows row-index (- elem-index 1) )
     (get-matrix-elem-by-indexes rows row-index (+ elem-index 1) )
     (get-matrix-elem-by-indexes rows (+ row-index 1) (+ elem-index 1) )
     (get-matrix-elem-by-indexes rows (+ row-index 1) (- elem-index 1) )
     (get-matrix-elem-by-indexes rows (- row-index 1) (- elem-index 1) )
     (get-matrix-elem-by-indexes rows (- row-index 1) (+ elem-index 1) )		 
     )
  )
	
(define (get-matrix-elem-by-indexes rows x y) 
  (cond ( (or (< x 1) (< y 1)) 0)
        ( (or (> x (length rows)) (> y (length rows))) 0)
        (else (get-elem-at-index y (get-elem-at-index x rows) ) )
        )
  )

;Printing the board
(define (print-board board)
  (when (not (null? board))
    (display (car board))
    (newline)
    (print-board (cdr board)) 
    )
  )

;Segment for launching
(define (conway-game board reps)
  (when (> reps 0)
    (print-board board)
    (newline)
    (conway-game (next-step board) (- reps 1) ) 
    )
  )
  
 ;example usage
 (conway-game '(
 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
 (0 0 0 0 0 1 0 1 0 1 0 0 0 0 0)
 (0 0 0 0 0 1 0 0 0 1 0 0 0 0 0)
 (0 0 0 0 0 1 0 0 0 1 0 0 0 0 0)
 (0 0 0 0 0 1 0 0 0 1 0 0 0 0 0)
 (0 0 0 0 0 1 0 1 0 1 0 0 0 0 0)
 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
) 10)
 
