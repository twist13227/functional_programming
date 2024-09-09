#lang scheme/base

(define (even-fib-list-a n)
  (
    let loop( (i n) (fst 0) (snd 1) (lst '()))
      (
        cond ((<= i 0) (reverse lst))
        ((even? fst) (loop (- i 1) snd (+ fst snd) (cons fst lst)))
        (else  (loop i snd (+ fst snd) lst))
      )
  )
)  
(even-fib-list-a 5)


(define (even-fib-list-b n)
  (define (fib i)
    (cond
      ((= i 0) 0)
      ((= i 1) 1)
      (else (+ (fib (- i 1)) (fib (- i 2))))))

  (let even-fibs( (i 1) (current 1) (next 0) (lst '()) (count 0))
    (cond
      ((>= count n) (reverse lst))
      ((and (= i 1) (= current 0)) (even-fibs (+ i 1) next (+ current next) (cons current lst) (+ count 1)))
      ((even? current) (even-fibs (+ i 1) next (+ current next) (cons current lst) (+ count 1)))
      (else (even-fibs (+ i 1) next (+ current next) lst count))))

 )
 
(even-fib-list-b 5)
