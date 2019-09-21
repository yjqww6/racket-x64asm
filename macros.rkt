#lang typed/racket/base
(require (for-syntax racket/base syntax/parse)
         typed/racket/unsafe
         "private/operand.rkt"
         "private/operand-helper.rkt"
         "private/assembler.rkt"
         "private/emit.rkt")

(provide define-cast λ! define-λ!)

(define-syntax define-cast
  (syntax-parser
    [(_ name:id
        (~alt (~once (~seq #:ctype ctype))
              (~optional (~seq #:requires (r ...)) #:defaults ([(r 1) '()]))
              (~once (~seq #:type T)))
        ...)
     #`(begin
         (module name racket/base
           (begin
             (require #,(datum->syntax #f 'racket/base)
                      #,(datum->syntax #f 'ffi/unsafe) r ...)
             (define id
               #,(datum->syntax
                  #f
                  `(λ (p)
                     (function-ptr (cast p _uintptr _pointer)
                                   ,(syntax->datum #'ctype)))))
             (provide id)))
         (unsafe-require/typed 'name
                               [(id name) (Nonnegative-Fixnum -> T)]))]))

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