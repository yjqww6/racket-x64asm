#lang typed/racket/base
(require (for-syntax racket/base syntax/parse)
         typed/racket/unsafe
         "../main.rkt")

(provide define-cast λ! define-λ!)

(module castme racket/base
  (require ffi/unsafe)
  (define-namespace-anchor ac)
  (define ns (namespace-anchor->namespace ac))
  (define (make-cast ct)
    (λ (p)
      (function-ptr (cast p _uintptr _pointer)
                    (eval ct ns))))
  (provide make-cast))

(unsafe-require/typed 'castme
                      [make-cast (All (a) (Any -> (Nonnegative-Fixnum -> a)))])

(define-syntax define-cast
  (syntax-parser
    [(_ name:id
        (~alt (~once (~seq #:ctype ctype))
              (~optional (~seq #:requires (r ...)) #:defaults ([(r 1) '()]))
              (~once (~seq #:type T)))
        ...)
     #'(define name ((inst make-cast T) 'ctype))]))

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
