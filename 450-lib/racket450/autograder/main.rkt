#lang racket/base

(provide (except-out
          (all-from-out
           rackunit
           racket450)
          #%top
          #%module-begin)
         DECLARE-HW-NUM
         test-case
         (rename-out [top450 #%top]
                     [testing-mb450 #%module-begin]))

(require rackunit
         rackunit/text-ui
         racket450
         racket/format
         (for-syntax racket/base
                     syntax/stx
                     syntax/parse
                     racket/syntax))

(require rackunit
         (only-in rackunit [test-case ru:test-case]))

(struct exn:fail:contract:dynamic-require exn:fail:contract ())

(define HW-NUM
  (make-parameter #f)
  #;(lambda ()
     (raise
      (exn:fail:contract
       "HW file not declared (are you using #lang racket/testing ?)"))))

(define-syntax DECLARE-HW-NUM
  (syntax-parser [(_ f) #'(HW-NUM f)]))

(define (HW-FILE)
  (format "hw~a.rkt" (HW-NUM)))
(define (TEST-FILE)
  (format "hw~a-tests.rkt" (HW-NUM)))

(define-syntax (HW stx)
  (syntax-parse stx
    [(_ symb)
     #'(dynamic-require
        (HW-FILE)
        'symb
        (lambda ()
          (raise
           (exn:fail:contract:dynamic-require
            (format
             "attempted to use an identifier that was not defined: ~a" 'symb)
            (current-continuation-marks)))))]))

(define-syntax (top450 stx)
  (syntax-parse stx
    [(_ . p)
;     #:do[(displayln (syntax->datum #'p))]
     #'(HW p)]))

(define-syntax (test-case stx)
  (syntax-parse stx
    [(_ nam chk ...)
     #'(ru:test-case nam
                     (with-check-info*
                         (list
;                          (make-check-location (list (HW-FILE) #f #f #f #f))
                          (make-check-name nam)
                          (make-check-expression 'chk))
                       (lambda ()
                         (with-handlers
                             ([exn:fail:contract:dynamic-require?
                               (lambda (e)
                                 (fail (exn-message e)))]
                              [exn:fail:contract?
                               (lambda (e)
                                 (fail (exn-message e)))])
                           chk))) ...)]))

;; format of a #lang racket450/autograder file (in any order)
;; (DECLARE-HW-NUM ...)
;; (requires ...)
;; (defines ...)
;; (test-cases ...)
(define-syntax testing-mb450
  (syntax-parser
    [(_ (~alt (~once
               (~describe #:role "DECLARE-HW-NUM" "hw num"
                          (~and hw-decl
                                ((~literal DECLARE-HW-NUM) _)))
               #:too-few
               "#lang racket450/autograder file missing DECLARE-HW-NUM")
              (~and req ((~literal require) . _))
              (~and def ((~or (~literal define)
                              (~literal define/contract)). _))
              (~and def-stx ((~literal define-syntax) . _))
             tst) ...
        ;tst ...)
        )
     #:with (tst-case ...)
            (stx-map
             (syntax-parser
               [((~literal test-case) . _)
                this-syntax]
               [this-tst
                (syntax/loc this-syntax
                  (test-case (~a 'this-tst) this-tst))])
             #'(tst ...))
     #'(#%module-begin
        hw-decl
        req ...
        def ...
        def-stx ...
        (define TESTS
          (test-suite
           (string-append (HW-FILE) " TESTING")
           (test-case
            (string-append "!CRASH CHECK: "
                           (HW-FILE)
                           " (MUST PASS OR NO HW CREDIT)")
            (check-not-exn
             (lambda ()
               (dynamic-require
                (HW-FILE)
                #f
                (lambda ()
                  (raise
                   (exn:fail:contract:dynamic-require
                    (format "hw file ~a crashed" (HW-FILE))
                    (current-continuation-marks))))))))
           (test-case
            (string-append "!TEST FILE CRASH CHECK: "
                           (TEST-FILE)
                           " (MUST PASS OR NO HW CREDIT)")
            (check-not-exn
             (lambda ()
               (dynamic-require
                (TEST-FILE)
                #f
                (lambda ()
                  (raise
                   (exn:fail:contract:dynamic-require
                    (format "test file ~a crashed" (TEST-FILE))
                    (current-continuation-marks))))))))
           (test-case
            (string-append "!CHECK TEST SUITE DEFINED in: "
                           (TEST-FILE)
                           " (MUST PASS OR NO HW CREDIT)")
            (check-true
             (test-suite?
              (dynamic-require
               (TEST-FILE)
               'TESTS
               (lambda ()
                 (raise
                  (exn:fail:contract:dynamic-require
                   (format "TESTS test-suite not defined, using #lang racket450/testing?")
                   (current-continuation-marks))))))))

           (test-case
            (string-append "!CHECK ALL TESTS PASSING in: "
                           (TEST-FILE)
                           " (MUST PASS OR NO HW CREDIT)")
            (check-true
             (andmap
              test-success?
              (run-test
               (dynamic-require
                (TEST-FILE)
                'TESTS
                (lambda ()
                  (raise
                   (exn:fail:contract:dynamic-require
                    (format "TESTS test-suite not defined, using #lang racket450/testing?")
                    (current-continuation-marks)))))))))

           tst-case ...))
        (module+ main
          (require rackunit/text-ui)
          (run-tests TESTS 'verbose)))]))


