#lang racket/base
(require (for-syntax racket/base syntax/parse)
         "private/operand.rkt"
         "private/operand-helper.rkt"
         "private/assembler.rkt"
         "private/emit.rkt")

(provide λ! define-λ!)

(define-syntax λ!
  (syntax-parser
    [(_ cast:expr
        (~alt
         (~optional (~seq #:labels (l ...))
                    #:defaults ([(l 1) '()]))
         (~optional (~seq #:assembler asm)
                    #:defaults ([asm #'(current-assembler)]))
         (~optional (~and #:captured cap)))
        ...
        body ...+)
     #'(with-labels (~? cap) (a l ...)
         (let ([c (make-context)]
               [e asm])
           (parameterize ([current-context c])
             (let ()
               (:! a)
               body ...)
             (emit-code! e c))
           (cast (label-addr a))))]))

(define-syntax define-λ!
  (syntax-parser
    [(_ name:id thing ...)
     #'(define name
         (λ! thing ...))]))