#lang racket
(require math/number-theory)

(define (get-dividers x) (
    let loop ((k 1) (result '())) (
        cond ((> k x) result)
             ((divides? k x) (loop (+ k 1) (cons k result)))
             (else (loop (+ k 1) result))
    )
))


(define (is-deficient? n) (
    < (foldl + 0 (get-dividers n)) (* 2 n)
))


(define (odd-deficient n) (
    let loop ((found 0) (k 1)) (
        if (is-deficient? k)
            (let ((new-found (+ found 1))) (
                if (equal? new-found n)
                    k
                    (loop new-found (+ k 2))
            ))
            (loop found (+ k 2))
    )
))


(define hash-table (make-hash '()))


(define (memo-odd-deficient n) (
    let* ((key (min n (foldl max 0 (hash-keys hash-table))))
          (val (hash-ref hash-table key 1))) (
              let loop ((hash-key (max 0 (- key 1))) (val val)) (
                 if (is-deficient? val)
                     (let ((new-key (+ hash-key 1))) (
                         begin
                         (hash-set! hash-table new-key val) 
                         (if (equal? new-key n) val (loop new-key (+ val 2)))
                     ))
                     (loop hash-key (+ val 2))
              )
          )
))


(println 'non-memo)
(odd-deficient 30)
(odd-deficient 30)
(odd-deficient 30)
(odd-deficient 30)
(odd-deficient 30)
(println 'memo)
(memo-odd-deficient 30)
(memo-odd-deficient 30)
(memo-odd-deficient 30)
(memo-odd-deficient 30)
(memo-odd-deficient 30)
(memo-odd-deficient 31)
(memo-odd-deficient 32)
(memo-odd-deficient 33)
(memo-odd-deficient 34)
(memo-odd-deficient 35)
(memo-odd-deficient 5)
