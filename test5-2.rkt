#lang racket

(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #())) ;

(define (task-5 tree h) (
    call/cc (lambda (cc-exit) (
        let loop((tree tree) (h h)) (
            if (empty-tree? tree)
                (if (= h 0) #t (cc-exit #f))
                (if (and (empty-tree? (tree-left tree)) (empty-tree? (tree-right tree))) 
                        (if (= h 1) #t (cc-exit #f))
                        (and (loop (tree-left tree) (- h 1)) (loop (tree-right tree) (- h 1)))
                )
        )     
    ))
))

;(task-5 #() 0)
;(task-5 #(1 #() #()) 1)
;(task-5 #() 1)