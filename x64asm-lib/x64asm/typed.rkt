#lang typed/racket/base
(require (for-syntax racket/base syntax/parse)
         typed/racket/unsafe)

(provide define-cast)

(define-syntax define-cast
  (syntax-parser
    [(_ name:id
        (~alt (~once (~seq #:ctype ctype))
              (~once (~seq #:type T)))
        ...)
     #`(begin
         (module name racket/base
           (begin
             (require #,(datum->syntax #f 'racket/base)
                      #,(datum->syntax #f 'ffi/unsafe))
             (define id
               #,(datum->syntax
                  #f
                  `(λ (p)
                     (function-ptr (cast p _uintptr _pointer)
                                   ,(syntax->datum #'ctype)))))
             (provide id)))
         (unsafe-require/typed 'name
                               [(id name) (Nonnegative-Fixnum -> T)]))]))
