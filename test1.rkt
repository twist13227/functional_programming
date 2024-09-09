#lang racket/base
(define (coplanar? x0 y0 z0 x1 y1 z1 x2 y2 z2)
  (= (+ (- (* x0 (- (* y1 z2) (* z1 y2)))
        (* y0 (- (* x1 z2) (* z1 x2))))
        (* z0 (- (* x1 y2) (* y1 x2)))) 0))

(coplanar? 1 0 0 0 1 0 0 0 1)