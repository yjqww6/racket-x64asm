#lang typed/racket
(require x64asm)

(define-cast int->int
  #:type (Integer -> Integer)
  #:ctype (_fun _int64 -> _int64))
          
(define-λ! fib int->int #:labels (start l1 l2 l3)
  (define arg0 (if (eq? (system-type)'windows) rcx rdi))
  (:! start)
  (push rbp)
  (mov rbp rsp)
  (sub rsp (imm8 16))
            
  (cmp arg0 (imm8 2))
  (jg (rel8 l1))
  (mov rax (imm32 1))
  (leave)
  (ret)
            
  (:! l1)
  (sub arg0 (imm8 1))
  (mov (mref 64 rbp - 8) arg0)
  (call (rel32 start))
  (mov (mref 64 rbp - 16) rax)
  (mov arg0 (mref 64 rbp - 8))
  (sub arg0 (imm8 1))
  (call (rel32 start))
  (add rax (mref 64 rbp - 16))
  (leave)
  (ret)
  )