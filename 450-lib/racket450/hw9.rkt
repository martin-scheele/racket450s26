#lang racket

(provide hw9-slice
         hw9-ref
         (rename-out [mk-array+ hw9-mk-array+]
                     [exn:fail:cs450:broadcast?
                      hw9-exn:fail:cs450:broadcast?]))

;; An Index is
;; - nonneg int
;; - neg int
;; Represents:
;; - nonneg: 0-based index for Array,
;; - neg: counts from the back where -1 = last element

(define (PosIndex? x) (exact-nonnegative-integer? x))

(define (Index? x)
  (and (integer? x)
       (or (negative? x)
           (PosIndex? x))))

;; neg step value unsupported, for now
(define (Step? x) (exact-nonnegative-integer? x))

;; An Atom is one of:
;; - Number
;; - Boolean
;; - String

(define (Atom? x)
  (or (number? x)
      (boolean? x)
      (string? x))) ; NOTE: not NumPy

;; An Array is an:
;; - Atom
;; - ArrayList
;; Represents: multidimensional data similar to NumPy array
;; invariant: is rectangular, ie not jagged

;; An ArrayList is one of
;; - empty
;; - (cons Array ArrayList)

(define (Array? x)
  (or (Atom? x)
      (ArrayList? x)))

(define (ArrayList? x)
  (list? x))

;; A Dim is an exact-nonnegative-int
(define (Dim? x) (exact-nonnegative-integer? x))

(define/contract (shape a)
  (-> Array? (listof Dim?))
  (cond
    [(Atom? a) empty]
    [else (shape-alst a)]))
(define/contract (shape-alst a)
  (-> ArrayList? (listof Dim?))
  (cond
    [(empty? a) (list 0)]
    [else
     (cons (length a) (shape (first a)))]))

(struct exn:fail:cs450:broadcast exn:fail [])

;; broadcast possible if shapes have equal length or one has dimension 1
(define/contract (can-broadcast? s1 s2)
  (-> (non-empty-listof Dim?) (non-empty-listof Dim?) boolean?)
  (or (= (first s1) (first s2))
      (= (first s1) 1)
      (= (first s2) 1)))

;; stretches Array a01 onto a02 for arithmetic operation,
;; according to NumPy algorithm
;; https://numpy.org/doc/stable/user/basics.broadcasting.html
(define/contract (broadcast a01 a02)
  (-> Array? Array? Array?)
  ;; Accumulator s1 : shape of a01 reversed
  ;; invariant: remaining dimensions must have counterpart in a2
  ;; Accumulator s2 : shape of a02 reversed
  ;; invariant: remaining dimensions must have counterpart in a1
  (define/contract (broadcast/a a1 a2 s1 s2)
    (-> Array? Array? (listof Dim?) (listof Dim?) Array?)
    (cond
      [(and (empty? s1) (empty? s2)) a1]
      [(and (empty? s1) (not (empty? s2)))
       (broadcast/a (list a1) a2 s1 (rest s2))]
      [(and (not (empty? s1)) (empty? s2))
       (broadcast/a a1 (list a2) (rest s1) s2)]
      [else
       (if (can-broadcast? s1 s2)
           (broadcast/a a1 a2 (rest s1) (rest s2))
           (raise
            (exn:fail:cs450:broadcast
             (format
              "ValueError: operands could not be broadcast together with shapes ~a ~a"
              (shape a01) (shape a02))
             (current-continuation-marks))))]))

  (broadcast/a a01 a02 (reverse (shape a01)) (reverse (shape a02))))

;; creates a mk-array+ fn, given a "+" function for array elements
(define/contract ((mk-array+ array-elt+) a1 a2)
  (-> (-> Atom? Atom? Atom?) (-> Array? Array? Array?))
  (cond
    [(and (Atom? a1) (Atom? a2)) (array-elt+ a1 a2)]
    [else (arraylist+ (broadcast a1 a2)
                      (broadcast a2 a1)
                      #:array-elt+ array-elt+)]))

(define/contract (array+/nobroadcast a1 a2 #:array-elt+ [array-elt+ +])
  (->* (Array? Array?) (#:array-elt+ (-> Atom? Atom? Atom?)) Array?)
  (cond
    [(and (Atom? a1) (Atom? a2)) (array-elt+ a1 a2)]
    [else (arraylist+ a1 a2)]))

;; outputs "rest" of a1, unless it has 1 dim and a2 doesnt
;; (compatibility comes from NumPy + algorithm)
(define/contract (rest-unless-len1compat a1 a2)
  (-> ArrayList? ArrayList? ArrayList?)
  (if (and (= 1 (length a1)) (not (= 1 (length a2))))
      a1
      (rest a1)))

(define/contract (arraylist+ a1 a2  #:array-elt+ [array-elt+ +])
  (->* (ArrayList? ArrayList?) (#:array-elt+ (-> Atom? Atom? Atom?)) ArrayList?)
  (cond
    [(and (empty? a1) (empty? a2)) empty]
    [else
     (cons (array+/nobroadcast (first a1) (first a2) #:array-elt+ array-elt+)
           (arraylist+ (rest-unless-len1compat a1 a2)
                       (rest-unless-len1compat a2 a1)
                        #:array-elt+ array-elt+))]))

(define/contract (hw9-ref a i)
  (-> Array? Index? Array?)
  (ref/flat a i))

(define/contract (ref/flat a . indices)
  (->* (Array?) () #:rest (listof Index?) Array?)
  (cond
    [(empty? indices) a]
    [else
     (apply ref/flat
            (list-ref a (index-fn (first indices) (length a)))
            (rest indices))]))

;; for inclusive, ie non-stop indices
;; n is max exclusive value of i, i.e., length of outer Array dim
;; better name? nonneg-idx
(define/contract (index-fn i n)
  (-> Index? exact-nonnegative-integer? PosIndex?)
  (cond
    [(exact-nonnegative-integer? i) i]
    [(negative? i) (+ n i)]))

;; stop is exclusive
(define/contract (hw9-slice a start stop step)
  (-> ArrayList? Index? Index? Step? ArrayList?)
  (get-slice a start stop #:step step))

;; get-slice : arraylist
(define/contract (get-slice a i [j (length a)] #:step [step 1])
  (->* (ArrayList? Index?) (Index? #:step Step?) ArrayList?)
  (define pos-i (index-fn i (length a)))
  (define pos-j (index-fn j (length a)))
  (define dropped (drop a pos-i))
  (define taked (take dropped (- pos-j pos-i)))
  (filter/step taked step))

;; filters list such that only every "step" elements remain
;; first element is always kept
(define/contract (filter/step lst step)
  (-> list? Step? list?)

  ;; accumulator curr-step, represents num elts left to skip
  ;; invariant: curr-step < step
  (define/contract (filter-step/a lst curr-step step)
    (-> list? Step? Step? list?)
    (cond
      [(empty? lst) lst]
      [else
       (if (= curr-step 1)
           (cons (first lst) (filter-step/a (rest lst) step step))
           (filter-step/a (rest lst) (sub1 curr-step) step))]))

  (filter-step/a lst 1 step))
