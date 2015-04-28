#lang racket

(require rackunit)

;; (define-alias Reducer (TFn 
;; (define-alias Transducer

;; A transducer is a transformation from one reducing function to another.

(define map
  (case-lambda
    [(f) (lambda (rf)
           (case-lambda
             ([] (rf))
             ([result] (rf result))
             ([result input]
              (rf result (f input)))
             ([result input . inputs]
              (rf result (apply f input inputs)))))]))

(define transduce
  (case-lambda
    ([xform f init coll]
     (let* ([f (xform f)]
            [ret (foldl f init coll)])
       (f ret)))))
     

(check-equal? (transduce (compose (map add1) (map add1)) + 0 '(1 2 3))
              12)
