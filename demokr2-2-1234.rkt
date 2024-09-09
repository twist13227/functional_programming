#lang racket

(define-syntax when
    (syntax-rules ()
        ((when test) test)
        ((when test expr ...)
            (if test (begin expr ...) #f)
        )
    )
)

(when #t)
(when #f)
(when #t (print 'a) (print 'b) (println 'c))
(when #f (print 'a) (print 'b) (println 'c))

(define (filter1 f lst)
  (reverse (foldl (lambda (x y) ( if (f x) (cons x y) y )) null lst)))

(define (filter2 f lst)
  (foldr (lambda (x y) (if (f x) (append (list x) y) y)) null lst))

(filter1 even? '(1 2 3 4 5))
(filter2 even? '(1 2 3 4 5))

;  (λz. (λx. ((λy. (x z)) ((λy. y y) (λy. y y z))))) a b
;  (λz. (λx. ((λy. (x z)) ((λt. t t) (λk. k k z))))) a b
;  (λx. ((λy. (x a)) ((λt. t t) (λk. k k a)))) b
;  ((λy. (b a)) ((λt. t t) (λk. k k a)))
;  вместо y подставляется ((λt. t t) (λk. k k a))
;  но в (b a) нету y, поэтому ответом будет просто (b a)
;  Ответ: (b a) - нормальная форма


(define (nthbit n) (
    let loop ((x 1)) ( if (= x n) 1 (
        if (> x n) 0 ( loop (* x 3))
    ))
))

(nthbit 1)
(nthbit 2)
(nthbit 3)
(nthbit 4)
(nthbit 5)
(nthbit 6)
(nthbit 7)
(nthbit 8)
(nthbit 9)
(nthbit 10)