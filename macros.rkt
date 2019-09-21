#lang racket/base
(require (for-syntax racket/base syntax/parse)
         "private/operand.rkt"
         "private/operand-helper.rkt"
         "private/assembler.rkt"
         "private/emit.rkt")

(provide λ! define-λ!)

(define-syntax λ!
  (syntax-parser
    [(_ cast:expr (~optional (~seq #:labels (l ...))
                             #:defaults ([(l 1) '()]))
        body ...+)
     #'(with-labels (#:entry a l ...)
         (let ([c (make-context)])
           (parameterize ([current-context c])
             (let ()
               (:! a)
               body ...)
             (emit-code!)))
         (cast (find-entry a)))]))

(define-syntax define-λ!
  (syntax-parser
    [(_ name:id thing ...)
     #'(define name
         (λ! thing ...))]))