#lang racket

(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))


(define (print-level-x tree x) (
    let loop ((t tree) (level 0) (cc (lambda () (void)))) (
        if (or (empty-tree? t) (> level x))
            (cc)
            (if (= level x)
                (begin 
                    (print (tree-data t))
                    (write-char #\space)
                    (cc)
                )
                (loop (tree-right t) (+ level 1) (lambda ()(loop (tree-left t) (+ level 1) cc)))
            )
    )
))


(define (get-max-level tree cc) (
    if (empty-tree? tree)
        (cc 0)
        (get-max-level (tree-left tree)(lambda (y) (get-max-level (tree-right tree)(lambda (z) (cc (+ 1 (max y z)))))))
))


(define (print-tree-by-level-desc tree) (
    let ((max-level (get-max-level tree (lambda (x) x)))) (
        let loop ((tree tree)(x max-level)) (
            if (< x 0)
                (void)
                (begin 
                    (print-level-x tree x )
                    (newline)
                    (loop tree (- x 1))
                )
        )
    )
))

;(print-tree-by-level-desc #())
;(print-tree-by-level-desc #(1 #() #())) 
;(print-tree-by-level-desc #(10 #(21 #() #()) #(22 #() #()))) 
;(print-tree-by-level-desc #(10 #(21 #(31 #() #()) #()) #(22 #(33 #() #()) #(34 #() #())))) 