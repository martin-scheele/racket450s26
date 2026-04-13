#lang racket

(provide hw9-slice
         (rename-out
          [mk-Slice/testing hw9-mk-Slice]
          [Slice? hw9-Slice?]
          [mk-array+ hw9-mk-array+]
          [exn:fail:cs450:broadcast?
           hw9-exn:fail:cs450:broadcast?]))

;; An Index is one of:
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

;; Array -> exact-nonneg-int
(define (ndim a)
  (cond
    [(Atom? a) 0]
    [else (ndim-alst a)]))

;; arraylist -> exact nonneg int
(define (ndim-alst a)
  (cond
    [(empty? a) 1]
    [else (+ 1 (ndim (first a)))]))

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
    [else (arraylist+ a1 a2 #:array-elt+ array-elt+)]))

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

;; a Slice is a
;; index
;; (mk-Slice start MaybeStop step)
;; - start and stop can be neg, step cannot
;; - start is inclusive, stop is exclusive
;; Represents: Slice range
(define (Slice? x)
  (or (Index? x) (SliceRange? x)))

;; A MaybeStop is either #f or an Index (exclusive)
(define (MaybeStop? x) (or (false? x) (Index? x)))

(struct SliceRange [i j step] #:transparent)
;; careful! stop is exclusive, so = -1 is not the same as #f
(define/contract (mk-Slice i [j #f] #:step [step 1])
  (->* (Index?) (MaybeStop? #:step Step?) Slice?)
  (SliceRange i j step))

(define/contract (mk-Slice/testing #:start [start 0]
                                   #:stop [stop #f]
                                   #:step [step 1])
  (->* () (#:start Index? #:stop MaybeStop? #:step Step?) Slice?)
  (mk-Slice start stop #:step step))

(define/contract (hw9-slice a slices)
  (-> Array? (listof Slice?) Array?)
  (apply slice a slices))

(define/contract (slice a . slices)
  (->* (Array?) () #:rest (listof Slice?) Array?)
  (cond
    [(empty? slices) a]
    [else
     (define slice-res
       (slice1 a (first slices)))
     (if (<= (ndim slice-res) 1)
         (apply slice slice-res (rest slices))
         (map
          (lambda (aa)
            (apply slice aa (rest slices)))
          slice-res))]))

;; sets Stop index to len if #f or > len
(define/contract (adjust-stop j len)
  (-> MaybeStop? exact-nonnegative-integer? Index?)
  (cond
    [(false? j) len]
    [else (if (> j len) len j)]))

(define/contract (slice1 a s)
  (-> Array? Slice? Array?)
  (match s
    [(? number? s) (ref/flat a s)]
    [(SliceRange i j step)
     (get-slice a i (adjust-stop j (length a)) #:step step)]))

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

(module+ test
  (require rackunit)
  ;; check auto array stop truncate
  (check-equal?
   (hw9-slice '[[1 2 3 4] [5 6 7 8]]
              (list
               (mk-Slice/testing #:start 0 #:stop 3)))
   '[[1 2 3 4]
     [5 6 7 8]])
  
  ;; check auto array stop truncate
  (check-equal?
   (hw9-slice '[[1 2 3 4] [5 6 7 8]]
              (list
               (mk-Slice/testing #:start 0 #:stop 3 #:step 2)))
   '[[1 2 3 4]]))