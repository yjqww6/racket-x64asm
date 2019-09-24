#lang typed/racket
(require x64asm)

(define-cast adder
  #:type (Fixnum -> Fixnum)
  #:ctype (_fun _int -> _int))

(define-cast adder-get
  #:type (-> Fixnum)
  #:ctype (_fun -> _int))

(define arg0 (if (eq? (system-type) 'windows) edi ecx))

(define (make-adder/get [init : Integer])
  (parameterize ([current-context (make-context)])
    (with-labels #:captured ()
      
      (:! (label inc))
      (mov rax (imm64 (label d)))
      (add (mref 32 rax) arg0)
      (mov eax (imm32 init #:! (label d)))
      (ret)
      
      (:! (label get))
      (mov eax (moff 32 (imm64 (label d))))
      (ret)
      
      (emit-code!)
      (values (adder (label-addr (label inc)))
              (adder-get (label-addr (label get)))))))

