#lang typed/racket/base
(require racket/match
         (for-syntax racket/base syntax/parse racket/syntax)
         "helper.rkt")

(provide (except-out (all-defined-out)
                     define-reg define-xmm define-gpr))

(define-type Size (U 8 16 32 64 80 128 256))

(struct Reg ([name : Symbol] [code : Byte] [size : Size])
  #:transparent)

(struct GPR Reg ())
(struct XMM Reg ())

(define-struct-match ?Reg Reg name code size)
(define-struct-match ?GPR GPR name code size)
(define-struct-match ?XMM XMM name code size)

(define-syntax (define-reg stx)
  (syntax-parse stx
    [(_ T id:id code:number size:number)
     #'(begin
         (define reg (T 'id code (ann size size)))
         (define-match-expander id
           (λ (stx)
             (syntax-case stx ()
               [(_) #'(== reg)]
               [(_ s) #'(and (== reg)
                             (?Reg #:size s))]))
           (λ (stx)
             (syntax-case stx ()
               [a (identifier? #'a) #'reg]))))]))

(define-syntax (define-gpr stx)
  (syntax-parse stx
    [(_ [code:number n8:id n16:id n32:id n64:id ?P] ...)
     #'(begin
         (begin 
           (define-reg GPR n8 code 8)
           (define-reg GPR n16 code 16)
           (define-reg GPR n32 code 32)
           (define-reg GPR n64 code 64)
           (define-match-expander ?P
             (λ (stx)
               (syntax-case stx ()
                 [(_)
                  #'(or #;(n8) (n16) (n32) (n64))]
                 [(_ s)
                  #'(or (n16 s) (n32 s) (n64 s))]))))
         ...)]))

(define-reg GPR ah 4 8)
(define-reg GPR ch 5 8)
(define-reg GPR dh 6 8)
(define-reg GPR bh 7 8)

(define-gpr
  [0 al ax eax rax ?rAX]
  [1 cl cx ecx rcx ?rCX]
  [2 dl dx edx rdx ?rDX]
  [3 bl bx ebx rbx ?rBX]
  [4 spl sp esp rsp ?rSP]
  [5 bpl bp ebp rbp ?rBP]
  [6 sil si esi rsi ?rSI]
  [7 dil di edi rdi ?rDI]
  [8 r8b r8w r8d r8 ?r8]
  [9 r9b r9w r9d r9 ?r9]
  [10 r10b r10w r10d r10 ?r10]
  [11 r11b r11w r11d r11 ?r11]
  [12 r12b r12w r12d r12 ?r12]
  [13 r13b r13w r13d r13 ?r13]
  [14 r14b r14w r14d r14 ?r14]
  [15 r15b r15w r15d r15 ?r15])

(define (reg-require-rex? [x : Reg])
  (match x
    [(?GPR #:size 64) #b1000]
    [(or (spl) (bpl) (sil) (dil)) 0]
    [_ #f]))

(define (reg-code-lowbits [x : Reg])
  (bitwise-and (Reg-code x) #b111))

(define (reg-code-highbit [x : Reg])
  (arithmetic-shift (bitwise-and (Reg-code x) #b1000) -3))

(define-syntax (define-xmm stx)
  (with-syntax ([((name code) ...)
                 (for/list ([i (in-range 16)])
                   #`(#,(format-id (syntax-local-introduce #'define-xmm)
                                 "xmm~a" i)
                      #,i))])
    #'(begin
        (define name (XMM 'name code (ann 128 Size)))
        ...)))

(define-xmm)