#lang typed/racket
(require x64asm)

(define-cast dd->d
  #:type (Flonum Flonum -> Flonum)
  #:ctype (_fun _double _double -> _double))

(define my-fl+
  (parameterize ([current-context (make-context)])
    (define entry (label))
    (:! entry)
    (addsd xmm0 xmm1)
    (ret)
            
    (emit-code!)
    (dd->d (label-addr entry))))