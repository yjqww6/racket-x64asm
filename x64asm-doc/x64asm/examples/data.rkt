#lang typed/racket
(require x64asm)

(define-cast int->int
  #:type (Integer -> Integer)
  #:ctype (_fun _int64 -> _int64))

(define-λ! f int->int #:captured
  (define arg0 (if (eq? (system-type)'windows) rcx rdi))
  (mov rax (imm64 (label data)))
  (jmp (mref 64 rax + arg0 * 8))
  (:! (label here))
  (mov eax (imm32 100))
  (ret)
  (:! (label l1))
  (mov eax (imm32 200))
  (ret)
  (:! (label l2))
  (mov eax (imm32 300))
  (ret)

  (:! (label data))
  (data!
   (imm64 (label here))
   (imm64 (label l1))
   (imm64 (label l2))))