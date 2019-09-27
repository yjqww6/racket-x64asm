#lang typed/racket
(require x64asm)
(define-cast ->int
  #:type (-> Integer)
  #:ctype (_fun -> _int))
(define-λ! get-1000 ->int
  (mov rax (imm32 1000))
  (ret))