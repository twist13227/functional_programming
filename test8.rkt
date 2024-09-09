#lang racket

(define (stream-scale s t)
    (stream-map (lambda (x) (* x t)) s)
)

(define (powers x)
    (stream-cons 1 (stream-scale (powers x) x))
)

(define 3-and-7-powers-stream (
    let loop ((3-stream (powers 3)) (7-stream (stream-rest (powers 7))))(
        let ((cur-3-stream (stream-first 3-stream)) (cur-7-stream (stream-first 7-stream))) (
            if (< cur-3-stream cur-7-stream)
                (stream-cons cur-3-stream (loop (stream-rest 3-stream) 7-stream))
                (stream-cons cur-7-stream (loop 3-stream (stream-rest 7-stream)))
        )
    )
))


(stream->list (stream-take 3-and-7-powers-stream 50))