#lang racket/base

(provide (except-out
          (all-from-out racket/base
                        racket/bool
                        racket/list
                        racket/math
                        racket/match
                        racket/format
                        racket/string
                        racket/random
                        racket/contract
                        racket/function
                        lang/htdp-advanced
                        rackunit)
          ;; excluded names:
          #%module-begin
          set!
          struct
          values
          define-values
;          match-define
;          match-let
;          let*
          letrec)
         local
         check-expect
         define-struct
         (rename-out
          [struct450 struct]
          [mb450 #%module-begin]))

(require racket/bool
         racket/list         
         racket/math
         racket/match
         racket/format
         racket/string
         racket/random
         racket/contract
         racket/function
         rackunit
         (only-in lang/htdp-advanced .. ....)
         (for-syntax "450-stx-utils.rkt"
                     racket/base
                     syntax/stx
                     syntax/parse
                     racket/syntax))

(define-syntax mb450
  (syntax-parser
    [(_ forms ...)

     #:with (non-test-forms ...)
     (filter
      (compose not test-form?)
      (stx->list #'(forms ...)))

     #:with (test-forms ...)
     (filter
      test-form?
      (stx->list #'(forms ...)))

     #:with (wrapped-test-forms ...)
     (stx-map
      maybe-wrap-test-form
      #'(test-forms ...))

     #:with do-provide-all (datum->syntax this-syntax '(provide (all-defined-out)))

     #'(#%module-begin
        do-provide-all
        non-test-forms ...
        test-forms ...
        #;(define HW-EXAMPLES
          (test-suite
           (string-append "HW Examples, run as extra Tests")
           (let () wrapped-test-forms ... (void))))
        #;(module+ main
          (require rackunit/text-ui)
          (run-tests HW-EXAMPLES 'verbose)))]))

;; override htdp forms with syntax error
(define-syntax define-struct
  (syntax-parser
    [(_ . _)
     #'(raise-syntax-error
        'define-struct "use struct instead of define-struct")]))

(define-syntax check-expect
  (syntax-parser
    [(_ . _)
     #'(raise-syntax-error
        'check-expect "use check-equal? instead of check-expect")]))

(define-syntax local
  (syntax-parser
    [(_ . _)
     #'(raise-syntax-error
        'local "use let or let* instead of local")]))

;; inserts #:transparent if not already present
(define-syntax struct450
  (syntax-parser
    [(_ (~and x (~not :keyword)) ...
        (~optional (~and #:transparent tr:keyword)
                   #:defaults ([tr #'#:transparent])))
     #'(struct x ... tr)]))
