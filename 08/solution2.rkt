#lang racket

(define file-contents
  (port->string (open-input-file "input") #:close? #t))

(define file-lines
  (string-split file-contents #rx"\r?\n"))

(define grid
  (map (lambda (line) (map (compose string->number string) (string->list line))) file-lines))

(define x-size (length grid))
(define y-size (length (car grid)))

;(println (string-append "grid dimensions: " (number->string x-size) "x" (number->string y-size)))

(define (get-at grd x y) (list-ref (list-ref grd x) y))

(define (view-distance-up grd x y)
  (view-distance-up-helper grd x y (get-at grd x y)))
(define (view-distance-up-helper grd x y height)
  (cond
    [(zero? x) 0]
    [(<= height (get-at grd (- x 1) y)) 1]
    [else (+ 1 (view-distance-up-helper grd (- x 1) y height))]))

(define (view-distance-down grd x y)
  (view-distance-down-helper grd x y (get-at grd x y)))
(define (view-distance-down-helper grd x y height)
  (cond
    [(= x (- x-size 1)) 0]
    [(<= height (get-at grd (+ x 1) y)) 1]
    [else (+ 1 (view-distance-down-helper grd (+ x 1) y height))]))

(define (view-distance-left grd x y)
  (view-distance-left-helper grd x y (get-at grd x y)))
(define (view-distance-left-helper grd x y height)
  (cond
    [(zero? y) 0]
    [(<= height (get-at grd x (- y 1))) 1]
    [else (+ 1 (view-distance-left-helper grd x (- y 1) height))]))

(define (view-distance-right grd x y)
  (view-distance-right-helper grd x y (get-at grd x y)))
(define (view-distance-right-helper grd x y height)
  (cond
    [(= y (- y-size 1)) 0]
    [(<= height (get-at grd x (+ y 1))) 1]
    [else (+ 1 (view-distance-right-helper grd x (+ y 1) height))]))

(define (scenic-score-at grd x y)
  (* (view-distance-up    grd x y)
     (view-distance-down  grd x y)
     (view-distance-left  grd x y)
     (view-distance-right grd x y)))

(define (best-score-in-row grd x) (best-score-in-row-helper grd x (- y-size 1)))
(define (best-score-in-row-helper grd x y)
  (if (negative? y)
      0
      (max (scenic-score-at grd x y) (best-score-in-row-helper grd x (- y 1)))))

(define (best-score grd) (best-score-helper grd (- x-size 1)))
(define (best-score-helper grd x)
  (if (negative? x)
      0
      (max (best-score-in-row grd x) (best-score-helper grd (- x 1)))))

;(for ([x (in-range x-size)])
;  (begin
;    (for ([y (in-range y-size)])
;      (display (scenic-score-at grid x y)))
;    (display "\n")))

(display (format "solution: ~a is the maximum possible scenic score" (best-score grid)))
