#lang racket

(define-syntax left-rot!(
    syntax-rules (buf)
    ((left-rot! buf cur-first m0) (
        begin (set! m0 cur-first)
    ))
    ((left-rot! buf cur-first m0 m1 m2 ...) (
        begin (set! m0 m1)
        (left-rot! buf cur-first m1 m2 ...)
    ))
    ((left-rot! m0) (void))
    ((left-rot! m0 m1 m2 ...) (
        let ((cur-first m0)) (
            begin (set! m0 m1)
            (left-rot! buf cur-first m1 m2 ...)
        )
    ))
))

(define a 1)
(define b 2)
(define c 3)
(define d 4)
(println a)
(println b)
(println c)
(println d)
(println '->)
(left-rot! a b c d)
(println a)
(println b)
(println c)
(println d)
(println 'done)
; 2 -> 2
(left-rot! a)
(println a)
(println 'done)
; 2 3 -> 3 2
(left-rot! a b)
(println a)
(println b)
(println 'done)
(let ((a 3) (b 2) (c 1)) (begin (left-rot! a b c) (/ a b c)))