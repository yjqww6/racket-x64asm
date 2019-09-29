#lang typed/racket/base
(require "private/emit.rkt"
         "private/assembler.rkt"
         "private/operand.rkt"
         "private/operand-helper.rkt"
         "private/registers.rkt"
         "private/macros.rkt"
         )

(provide (all-from-out "private/operand.rkt"
                       "private/operand-helper.rkt"
                       "private/registers.rkt"
                       "private/macros.rkt")
         current-assembler make-assembler assembler-shutdown-all!
         Assembler Assembler?
         current-context make-context
         Context Context?
         emit-code! label-addr :! data!)


(provide define-cast)

(require (for-syntax racket/base syntax/parse)
         typed/racket/unsafe)

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