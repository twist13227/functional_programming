#lang racket/base

(define Y
  (lambda (f) ((lambda (x) (x x))
               (lambda (g) (f (lambda args (apply (g g) args)))))))

(define n!!!!
    (lambda (i) ((Y (lambda (f) (lambda (n result)
            (case n
              [(0) result]
              [(1) result]
              [(2) (* 2 result)]
              [(3) (* 3 result)]
              [else (f (- n 4) (* n result))])))) i 1)
    )
)

(equal? (map n!!!! (build-list 20 (λ (x) x))) '(1 1 2 3 4 5 12 21 32 45 120 231 384 585 1680 3465 6144 9945 30240 65835))