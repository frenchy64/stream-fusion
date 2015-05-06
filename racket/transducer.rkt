#lang typed/racket/no-check

(require rackunit)

(define (conj xs x) (append xs `(,x)))

;; utils
(define identity (lambda (x) x))

;; reduce
(struct Reduced ([val : Any]))

(define reduced
  (lambda (val)
    (Reduced val)))

(define reduced?
  Reduced?)

(define deref-reduced
  (lambda (d)
    (Reduced-val d)))

(define (reduce f val coll)
  (if (pair? coll)
      (let ([v (f val (car coll))])
        (if (reduced? v)
            (deref-reduced v)
            (reduce f v (cdr coll))))
      val))

(define-type Reducer (All (a)
                       (All (r)
                            (r a -> r))))

;; A transducer is a transformation from one reducing function to another.

(define 
  #:forall (a b)
  (Tmap [f : (a -> b)]) : ((Reducer b) -> (Reducer a))
  (lambda (rf)
    (lambda ([result : r]
             [input : a])
      (rf result (f input)))))

(define 
  #:forall (a)
  (Tfilter pred)
  (lambda (rf)
    (lambda ([result : r]
             [input : a])
      (if (pred input)
          (rf result input)
          result))))

(define (transduce xform f init coll)
  (reduce (xform f) init coll))


;; equivalent to (+ (+ (+ 0 (add1 (add1 1)))
;;                     (add1 (add1 2)))
;;                  (add1 (add1 3)))
(check-equal? (transduce (compose (Tmap add1) (Tmap add1)) + 0 '(1 2 3))
              12)


;; (equivalent to (+ (+ (+ 0 (identity 1))
;;                      (identity 2))
;;                   (identity 3))
(check-equal? (transduce (Tmap identity) + 0 '(1 2 3))
              6)

;; equivalent to (+ 0 2)
(check-equal? (transduce (compose (Tfilter even?) (Tmap add1)) + 0 '(1 2 3))
              3)

;; equivalent to (+ (+ 0 2) 4)
(check-equal? (transduce (compose (Tmap add1) (Tfilter even?)) + 0 '(1 2 3))
              6)

(define small-list '(1 2 3 4))
(define big-list (for/list : Integer ([e : Integer 100000]) e))

;; one traversal over big-list
(define (one-trav) (transduce (compose (Tmap add1) (Tfilter even?)) + 0 big-list))

(time (for ([_ 100]) (one-trav)))

;; two traversals over big-list
(define (two-trav) (transduce (Tfilter even?) + 0 (map add1 big-list)))

(time (for ([_ 100])
        (two-trav)))

;; three traversals over big-list
(define (three-trav) (reduce + 0 (filter even? (map add1 big-list))))

(time (for ([_ 100]) (three-trav)))

(check-equal? #t (= 2500050000 (one-trav) (two-trav) (three-trav)))

(((Tmap add1) (lambda (r v) (cons v r))) '() 1)

;; Tmap is a transducer generator
(define step1 Tmap)

;; (Tmap add1) is a transducer (takes a reducing function and returns a reducing function)
(define step2 (step1 add1))

;; ((Tmap add1) conj) is a reducing function (that takes a list and an accumulator and returns a list)
(define step3 (step2 conj))

;; (((Tmap add1) conj) '() 1) 
(define step4.1 (step3 '() 1))
(define step4.2 (step3 step4.1 2))
(define step4.3 (step3 step4.2 3))
(define step4.4 (step3 step4.3 4))