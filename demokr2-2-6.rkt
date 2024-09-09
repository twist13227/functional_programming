#lang racket

(require srfi/41)  ; Для потоков

(define (merge-streams s1 s2)
  (stream-match s1
    [('() s2)]
    ((else (stream-match s2
            [('() s1)]
            ((else (let ((a (stream-first s1))
                         (b (stream-first s2)))
                     (if (< a b)
                         (stream-cons a (merge-streams (stream-rest s1) s2))
                         (stream-cons b (merge-streams s1 (stream-rest s2))))))))))
))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream)
)

(define stream-of-3s (stream-cons 1 (scale-stream stream-of-3s 3)))
(define stream-of-5s (stream-cons 1 (scale-stream stream-of-5s 5)))

(define (merge-powers m-stream n-stream)
  (if (stream-empty? m-stream)
      n-stream
      (merge-streams (scale-stream n-stream (stream-first m-stream))
                     (merge-powers (stream-rest m-stream) n-stream))))

(define stream3^m5^n (merge-powers stream-of-3s stream-of-5s))

; Показать первые 10 элементов для проверки
(stream->list (stream-take stream3^m5^n 10))