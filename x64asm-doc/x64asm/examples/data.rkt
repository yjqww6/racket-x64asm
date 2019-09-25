#lang typed/racket
(require x64asm)

(define-cast int->int
  #:type (Integer -> Integer)
  #:ctype (_fun _int64 -> _int64))

(define arg0 (if (eq? (system-type) 'windows) rcx rdi))

(define-λ! f int->int #:captured
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

;;; This one use a smaller table

(define-λ! g int->int #:captured
  (lea rdx (mref 64 rip + (rel32 (label data))))
  (xor r8 r8)
  (mov r8b (mref 8 rdx + arg0 * 1))
  (lea rax (mref 64 rip + (rel32 (label here))))
  (add rax r8)
  (jmp rax)
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
      
  (define (dist [a : Label] [b : Label])
    (latent-imm 8 (λ ()
                    (- (label-addr b)
                       (label-addr a)))))
  (data!
   (imm8 0)
   (dist (label here) (label l1))
   (dist (label here) (label l2))))