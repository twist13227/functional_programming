#lang scheme/base

(define (loop1 i fib-n-1 fib-n-2) 
(if (= i 0) '()
    (cons fib-n-1 (loop1 (- i 1) (+ fib-n-1 fib-n-2) fib-n-1))))

(define (list-fib-squares-a n)
  (map (lambda (x) (* x x)) (loop1 n 0 1)))

(list-fib-squares-a 10)


(define (loop2 i fib-n-1 fib-n-2)
(if (= i 0) fib-n-2
  (loop2 (- i 1) (+ fib-n-1 fib-n-2) fib-n-1)))

(define (list-fib-squares-b n)
  (map (lambda (x) (* x x)) (build-list n (lambda (i) (loop2 i 1 0)))))

(list-fib-squares-b 10)


(define (process lst) (
    let* 
    (
      (first-elem-mult (foldl * 1 (car lst)))
      (filter-function (lambda (some-list) (
        > (foldl + 0 some-list) first-elem-mult
      )))
    )
    (filter filter-function lst)
  )
)

(process '((5) (1 2) () (3 4) (2 3) (2 3 4)))