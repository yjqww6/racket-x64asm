#lang racket
(require ffi/unsafe x64asm/untyped)

;;; defines some helper functions to handle platform specific thing as much as possible
(define ptr-false
  (case (system-type 'gc)
    [(3m) 
     (cast #f _racket _uintptr)]
    [(cs) 6]))

(define notpairp
  (case (system-type 'gc)
    [(3m) 
     (λ (p)
       (mov r8w (mref 16 p))
       (cmp r8w (imm8 62))
       jne)]
    [(cs)
     (λ (p)
       (mov r8 p)
       (and r8b (imm8 7))
       (cmp r8b (imm8 1))
       jne)]))

(define notfixp
  (case (system-type 'gc)
    [(3m) 
     (λ (p)
       (mov r8 p)
       (and r8b (imm8 1))
       jz)]
    [(cs)
     (λ (p)
       (mov r8 p)
       (and r8b (imm8 7))
       jnz)]))

(define unfix
  (case (system-type 'gc)
    [(3m) 
     (λ (p)
       (sar p (imm8 1)))]
    [(cs)
     (λ (p)
       (sar p (imm8 3)))]))

(define fix
  (case (system-type 'gc)
    [(3m) 
     (λ (p)
       (shl p (imm8 1))
       (or p (imm8 1)))]
    [(cs)
     (λ (p)
       (shl p (imm8 3)))]))

(define arg0 (if (eq? (system-type) 'windows)
                 rcx
                 rdi))

(define-values (car-offset cdr-offset)
  (case (system-type 'gc)
    [(3m) (values 8 16)]
    [(cs) (values 7 15)]))

;;; A lightweight but flexible error handling approach using nop+disp32.


(define err-offset
  (let ([asm (make-assembler)]
        [ctx (make-context)]
        [l (label)]
        [i (label)])
    (:! l #:ctx ctx)
    (nop #:ctx ctx (mref 32 + (imm32 0 #:! i)))
    (emit-code! asm ctx)
    (assembler-shutdown-all! asm)
    (- (label-addr i) (label-addr l))))

(define (call/on-error target err)
  (call (rel32 target))
  (define here (label))
  (:! here)
  (nop (mref 32 + (latent-imm 32 (λ ()
                                   (- (label-addr err)
                                      (label-addr here)))))))

(define (ret-error)
  (pop r8)
  (mov r9d (mref 32 r8 + (imm8 err-offset)))
  (add r8 r9)
  (jmp r8))

;;; Since interior nodes don't do anything but pop on error in this example,
;;; an alternative approach may be recording the stack pointer at the beginning,
;;; restoring it when a object neighter pair nor fixnum is encountered,
;;; and then return with false.


(define-cast a->a
  #:type (Any -> Any)
  #:ctype (_fun _racket -> _racket))

(define-λ! sum-tree-fixnums a->a #:captured
  (push rbp)
  (mov rbp rsp)
  (call/on-error (label inner) (label err))
  (fix rax)
  (:! (label err))
  (leave)
  (ret)

  
  (:! (label inner))
  ((notfixp arg0) (rel8 (label not-fix)))
  
  (mov rax arg0)
  (unfix rax)
  (ret)

  (:! (label not-fix))
  ((notpairp arg0) (rel8 (label invalid)))

  (push arg0)
  (mov arg0 (mref 64 arg0 + (imm8 car-offset)))
  (call/on-error (label inner) (label child-err))
  (pop arg0)
  (mov arg0 (mref 64 arg0 + (imm8 cdr-offset)))
  (push rax)
  (call/on-error (label inner) (label child-err))
  (pop r8)
  (add rax r8)
  (ret)
  
  (:! (label child-err))
  (pop r8)
  (ret-error)
  
  (:! (label invalid))
  (mov rax (imm64 ptr-false))
  (ret-error))