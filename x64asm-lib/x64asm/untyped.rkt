#lang racket/base

(require racket/require (for-syntax racket/base syntax/parse)
         "main.rkt"
         ffi/unsafe)

(provide (all-from-out "main.rkt")
         define-cast)

(provide define-cast)

(define-syntax define-cast
  (syntax-parser
    [(_ name:id
        (~alt (~once (~seq #:ctype ctype))
              (~optional (~seq #:type T)))
        ...)
     #'(begin
         (define name
           (let ([tmp ctype])
             (λ (p)
               (function-ptr (cast p _uintptr _pointer)
                             tmp)))))]))
