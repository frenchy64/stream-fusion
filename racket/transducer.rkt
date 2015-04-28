#lang racket

(require rackunit)

;; utils
(define identity (lambda (x) x))

;; reduce
(struct Reduced (val))

(define reduced
  (lambda (val)
    (Reduced val)))

(define reduced?
  Reduced?)

(define deref-reduced
  (lambda (d)
    (Reduced-val d)))

(define reduce
  (case-lambda
    ([f val coll]
     (if (pair? coll)
         (let ([v (f val (car coll))])
           (if (reduced? v)
               (deref-reduced v)
               (reduce f v (cdr coll))))
         val))))

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

(define filter
  (case-lambda
    ([pred] (lambda (rf)
              (case-lambda
                ([] (rf))
                ([result] (rf result))
                ([result input]
                 (if (pred input)
                     (rf result input)
                     result)))))))

(define transduce
  (case-lambda
    ([xform f init coll]
     (let* ([f (xform f)]
            [ret (reduce f init coll)])
       (f ret)))))


;; equivalent to (+ (+ (+ 0 (add1 (add1 1)))
;;                     (add1 (add1 2)))
;;                  (add1 (add1 3)))
(check-equal? (transduce (compose (map add1) (map add1)) + 0 '(1 2 3))
              12)


;; (equivalent to (+ (+ (+ 0 (identity 1))
;;                      (identity 2))
;;                   (identity 3))
(check-equal? (transduce (map identity) + 0 '(1 2 3))
              6)

;; equivalent to (+ 0 2)
(check-equal? (transduce (compose (filter even?) (map add1)) + 0 '(1 2 3))
              3)

;; equivalent to (+ (+ 0 2) 4)
(check-equal? (transduce (compose (map add1) (filter even?)) + 0 '(1 2 3))
              6)