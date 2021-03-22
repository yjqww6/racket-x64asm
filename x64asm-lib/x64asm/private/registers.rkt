#lang typed/racket/base
(require racket/match
         (for-syntax racket/base syntax/parse racket/syntax)
         "helper.rkt")

(provide (except-out (all-defined-out)
                     define-reg define-xmm define-gpr))

(define-type Size (U 8 16 32 64 80 128 256))

(struct Reg ([name : Symbol] [code : Byte] [size : Size])
  #:transparent)

(struct GPR Reg () #:transparent)
(struct XMM Reg () #:transparent)
(struct Seg Reg ([prefix : Byte]) #:transparent)
(struct IP Reg () #:transparent)

(define-struct-match ?Reg Reg name code size)
(define-struct-match ?GPR GPR name code size)
(define-struct-match ?XMM XMM name code size)
(define-struct-match ?IP IP name code size)

(define-syntax (define-reg stx)
  (syntax-parse stx
    [(_ T id code:number size:number other ...)
     #'(define id (T 'id code (ann size size) other ...))]))

(define-reg IP rip 0 64)
(define-reg IP eip 0 32)

(define-syntax (define-gpr stx)
  (syntax-parse stx
    [(_ [code:number n8 n16 n32 n64] ...)
     #'(begin
         (begin 
           (define-reg GPR n8 code 8)
           (define-reg GPR n16 code 16)
           (define-reg GPR n32 code 32)
           (define-reg GPR n64 code 64))
         ...)]))

(define-reg GPR ah 4 8)
(define-reg GPR ch 5 8)
(define-reg GPR dh 6 8)
(define-reg GPR bh 7 8)

(define-gpr
  [0 al ax eax rax]
  [1 cl cx ecx rcx]
  [2 dl dx edx rdx]
  [3 bl bx ebx rbx]
  [4 spl sp esp rsp]
  [5 bpl bp ebp rbp]
  [6 sil si esi rsi]
  [7 dil di edi rdi]
  [8 r8b r8w r8d r8]
  [9 r9b r9w r9d r9]
  [10 r10b r10w r10d r10]
  [11 r11b r11w r11d r11]
  [12 r12b r12w r12d r12]
  [13 r13b r13w r13d r13]
  [14 r14b r14w r14d r14]
  [15 r15b r15w r15d r15])

(define (reg-require-rex? [x : Reg])
  (cond
    [(GPR? x)
     (define s (Reg-size x))
     (cond
       [(eq? s 64) #b1000]
       [(or (eq? x spl) (eq? x bpl) (eq? x sil) (eq? x dil)) 0]
       [else #f])]
    [else #f]))

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

(define-syntax-rule (define-seg [id code prefix] ...)
  (begin
    (define-reg Seg id code 16 prefix)
    ...))

(define-seg
  ;[cs 1 #x2e]
  ;[ds 3 #x3e]
  ;[es 0 #x36]
  [fs 4 #x64]
  [gs 5 #x65]
  ;[ss 2 #x36]
  )
