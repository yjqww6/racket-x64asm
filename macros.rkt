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
                    #:defaults ([asm #'#f]))
         (~optional (~and #:captured cap)))
        ...
        body ...+)
     #:with ret
     #'(with-labels (~? cap) (#:entry a l ...)
         (let ([c (make-context)])
           (parameterize ([current-context c])
             (let ()
               (:! a)
               body ...)
             (emit-code!)))
         (cast (find-entry a)))
     (if (syntax-e #'asm)
         #'(parameterize ([current-assembler asm])
             ret)
         #'ret)]))

(define-syntax define-λ!
  (syntax-parser
    [(_ name:id thing ...)
     #'(define name
         (λ! thing ...))]))