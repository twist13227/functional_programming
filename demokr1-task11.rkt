#lang racket

; 1 задание
(define (taskI lst)
  (let ((min-val (foldl min +inf.0 lst)))(
      cdr (foldl (lambda (x y)
                  (let ((fst (car y)) (other (cdr y)))
                    (cons (+ fst 1) (if (= x min-val) (cons fst other) other)
                    )
                  ))'(0) lst
          )
   )
))

;(taskI (list))
;(taskI (list 2 -1 2 3))
;(taskI (list -1 0 1 -1 0 1 -1))


; 2 задание
(define (taskII t s) (
    cond ((equal? t 1) s)
         ((equal? t 0) 0)
         (else (let ((quat_s (/ s 4))) (
                   +
                   (taskII (vector-ref t 0) quat_s)
                   (taskII (vector-ref t 1) quat_s)
                   (taskII (vector-ref t 2) quat_s)
                   (taskII (vector-ref t 3) quat_s)
               ))
         )
))
;(taskII #(1 0 0 #(1 1 1 0)) 16)


; 3 задание
; 4 задание
(define (taskIV-сс t s cc) (
    cond ((equal? t 1) s)
         ((equal? t 0) 0)
         (else (let ((quat_s (/ s 4)))(
                    taskIV-сс (vector-ref t 0) quat_s (lambda (x0) (
                        taskIV-сс (vector-ref t 1) quat_s (lambda (x1) (
                                taskIV-сс (vector-ref t 2) quat_s (lambda (x2) (
                                        taskIV-сс (vector-ref t 3) quat_s (lambda (x3) (cc (+ x3 x2 x1 x0)))
                                ))
                        ))
                    ))
         )))
))

;(taskIV-сс #(1 0 0 #(1 1 1 0)) 16 (lambda (x) x))


; 5 задание
(define (taskV . funcs) (
    cond ((empty? (cdr funcs)) (car funcs))
         (else (apply taskV (cons(lambda (x) ((car funcs)((cadr funcs) x)))(cddr funcs))))
))

;((taskV (lambda (x) (* x x)) (lambda (x) (+ x 3)) (lambda (x) (* x 5))) 2) ; (5*2+3)^2 = 13^2 = 169
;((taskV (lambda (x) (* x x))) 10) ; 10*10=100
;(taskV) ;fail
                                                                        