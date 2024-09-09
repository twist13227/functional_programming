#lang scheme/base
(require racket/class)


(define 2tree-interface<%> (interface () isEmpty? printTree))

(define Empty2tree%
  (class* object% (2tree-interface<%>)
    (super-new)
    (define/public (isEmpty?) #t)
    (define/public (printTree) '())
  )
)

(define Nonempty2tree%
  (class* object% (2tree-interface<%>)
    (super-new)
    (init-field tag)
    (init-field data)
    (field (left null))
    (field (right null))
    (define/public (isEmpty?) #f)
    (define/public (printTree)(
       begin
       (cond ((not (null? left)) (send left printTree)))
       (print tag)
       (newline)
       (cond ((not (null? right))(send right printTree)))
      ))
    (define/public (get-tag) tag)
    (define/public (get-data) data)
    (define/public (set-tag! t) (set! tag t))
    (define/public (set-data! d) (set! data d))
    (define/public (get-left) left)
    (define/public (get-right) right)
    (define/public (set-left! tr) (set! left tr))
    (define/public (set-right! tr) (set! right tr))
  )
)