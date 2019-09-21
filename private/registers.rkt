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
(struct Seg Reg ([prefix : Byte]))

(define-struct-match ?Reg Reg name code size)
(define-struct-match ?GPR GPR name code size)
(define-struct-match ?XMM XMM name code size)

(begin-for-syntax 
  (define-syntax-class reg
    (pattern name:id
             #:with up (datum->syntax
                        #'name
                        (string->symbol
                         (string-upcase
                          (symbol->string (syntax-e #'name))))))))

(define-syntax (define-reg stx)
  (syntax-parse stx
    [(_ T id:reg code:number size:number other ...)
     #'(begin
         (define id (T 'id code (ann size size) other ...))
         (define-match-expander id.up
           (λ (stx)
             (syntax-case stx ()
               [(_) #'(or (== id eq?)
                          (T 'id code size other ...))]
               [(_ s) #'(and (id.up)
                             (?Reg #:size s))]))))]))

(define-syntax (define-gpr stx)
  (syntax-parse stx
    [(_ [code:number n8:reg n16:reg n32:reg n64:reg ?P] ...)
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
                  #'(or (== n16 eq?)
                        (== n32 eq?)
                        (== n64 eq?)
                        (n16.up) (n32.up) (n64.up))]
                 [(_ s)
                  #'(and (?Reg #:size s) (?P))]))))
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
    [(or (SPL) (BPL) (SIL) (DIL)) 0]
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