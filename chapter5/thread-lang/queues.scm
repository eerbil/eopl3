(module queues (lib "eopl.ss" "eopl")

  (provide (all-defined-out))
  
  ;; queues

  ;; We maintain the queue by adding to the end and dequeuing from the
  ;; front. 

  ;; exercise: enqueue is expensive, since it uses append.  Do
  ;; something better than this.

  (define empty-queue
    (lambda ()
      '()))

  (define empty? null?)

  (define enqueue
    (lambda (q val)
      (append q (list val))))

  (define dequeue
    (lambda (q f)
      (f (car q) (cdr q))))
  
;this will check the remaining times and add the new element in its place.
(define insert
    (lambda (q val)
      (if (null? q)
          (enqueue q val)
          (if (> (caar q) (car val))
              (cons val q)
              (cons (car q) (insert (cdr q) val)))))) 

  )
