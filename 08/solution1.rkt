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

(define (is-visible-from-left grd x y)
  (is-visible-from-left-helper grd x y (get-at grd x y)))
(define (is-visible-from-left-helper grd x y height)
  (if (zero? y)
      #t
      (and (< (get-at grd x (- y 1)) height) (is-visible-from-left-helper grd x (- y 1) height))))

(define (is-visible-from-right grd x y)
  (is-visible-from-right-helper grd x y (get-at grd x y)))
(define (is-visible-from-right-helper grd x y height)
  (if (= y (- y-size 1))
      #t
      (and (< (get-at grd x (+ y 1)) height) (is-visible-from-right-helper grd x (+ y 1) height))))

(define (is-visible-from-top grd x y)
  (is-visible-from-top-helper grd x y (get-at grd x y)))
(define (is-visible-from-top-helper grd x y height)
  (if (zero? x)
      #t
      (and (< (get-at grd (- x 1) y) height) (is-visible-from-top-helper grd (- x 1) y height))))

(define (is-visible-from-bottom grd x y)
  (is-visible-from-bottom-helper grd x y (get-at grd x y)))
(define (is-visible-from-bottom-helper grd x y height)
  (if (= x (- x-size 1))
      #t
      (and (< (get-at grd (+ x 1) y) height) (is-visible-from-bottom-helper grd (+ x 1) y height))))

(define (is-visible grd x y)
  (or (is-visible-from-left   grd x y)
      (is-visible-from-right  grd x y)
      (is-visible-from-top    grd x y)
      (is-visible-from-bottom grd x y)))

(define (boolean->number b) (if b 1 0))

(define (count-visible-in-row-helper grd x y)
  (if (> y 0)
      (+
       (boolean->number (is-visible grd x (- y 1)))
       (count-visible-in-row-helper grd x (- y 1)))
      0))
(define (count-visible-in-row grd x) (count-visible-in-row-helper grd x y-size))

(define (count-visible-helper grd x)
  (if (> x 0)
      (+
       (count-visible-in-row grd (- x 1))
       (count-visible-helper grd (- x 1)))
      0))
(define (count-visible grd) (count-visible-helper grd x-size))

;(for ([x (in-range x-size)])
;  (begin
;    (for ([y (in-range y-size)])
;      (display (if (is-visible grid x y) "O" "X")))
;    (display "\n")))

(display (format "solution: ~a trees are visible from the edges" (count-visible grid)))
