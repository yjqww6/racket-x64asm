#lang racket/base

(require "private/operand-helper.rkt"
         racket/require (for-syntax racket/base syntax/parse)
         "main.rkt"
         ffi/unsafe)

(provide (all-from-out "main.rkt")
         define-cast λ! define-λ!)

(provide define-cast λ! define-λ!)

(define-syntax define-cast
  (syntax-parser
    [(_ name:id
        (~alt (~once (~seq #:ctype ctype))
              (~optional (~seq #:requires (r ...)) #:defaults ([(r 1) '()]))
              (~optional (~seq #:type T)))
        ...)
     #'(begin
         (require r) ...
         (define (name p)
           (function-ptr (cast p _uintptr _pointer)
                         ctype)))]))

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