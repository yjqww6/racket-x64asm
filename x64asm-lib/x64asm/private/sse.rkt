#lang typed/racket/base
(require "encode.rkt" "cases2.rkt" "assembler.rkt" "dispatch.rkt"
         "registers.rkt" "operand.rkt"
         racket/match
         (for-syntax racket/base syntax/parse
                     syntax/name racket/syntax))

(define-syntax-rule (V-W: opcode args ...)
  (G-E: '(#x0f opcode) args ...))
(define-syntax-rule (W-V: opcode args ...)
  (E-G: '(#x0f opcode) args ...))
(define-syntax-rule (V-I: opcode args ...)
  (E-I: '(#x0f opcode) args ...))
(define-syntax-rule (V-W-I: opcode args ...)
  (G-E-I: '(#x0f opcode) args ...))

(define-syntax-rule (define-VW128 p [name code] ...)
  (begin
    (define-dispatch name
      [(V-Wo) (V-W: code #:mandatory-prefix p)])
    ...))

(define-syntax-rule (define-VW128-VI8 p [name / code1 code2] ...)
  (begin
    (define-dispatch name
      [(V-Wo) (V-W: code1 #:mandatory-prefix p)]
      [(V-Ib) (V-I: code2 #:mandatory-prefix p #:/ /)])
    ...))

(define-syntax (define-pspd1 stx) 
  (syntax-parse stx 
    [(_ [name code] ...)
     #:with ((ps pd) ...) (map (λ (s) (list (format-id s "~aps" s)
                                            (format-id s "~apd" s)))
                               (syntax->list #'(name ...)))
     #'(begin
         (~@
          (define-dispatch ps
            [(V-Wo) (V-W: code)])
          (define-dispatch pd
            [(V-Wo) (V-W: code #:mandatory-prefix #x66)]))
         ...)]))

(define-syntax (define-pspd2 stx) 
  (syntax-parse stx 
    [(_ name
        [pred (encode ...)] ...)
     #:with ps (format-id #'name "~aps" #'name)
     #:with pd (format-id #'name "~apd" #'name)
     #'(begin
         (define-dispatch ps
           [pred (encode ...)]
           ...)
         (define-dispatch pd
           [pred (encode ... #:mandatory-prefix #x66)]
           ...))]))

(define-syntax (define-pdsd stx) 
  (syntax-parse stx 
    [(_ [name code] ...)
     #:with ((pd sd) ...) (map (λ (s) (list (format-id s "~apd" s)
                                            (format-id s "~asd" s)))
                               (syntax->list #'(name ...)))
     #'(begin
         (~@
          (define-dispatch pd
            [(V-Wo) (V-W: code #:mandatory-prefix #x66)])
          (define-dispatch sd
            [(V-Wq) (V-W: code #:mandatory-prefix #xf2)]))
         ...)]))

(define-syntax (define-psss stx) 
  (syntax-parse stx 
    [(_ [name code] ...)
     #:with ((ss ps) ...) (map (λ (s) (list (format-id s "~ass" s)
                                            (format-id s "~aps" s)))
                               (syntax->list #'(name ...)))
     #'(begin
         (~@
          (define-dispatch ps
            [(V-Wo) (V-W: code)])
          (define-dispatch ss
            [(V-Wd) (V-W: code #:mandatory-prefix #xf3)]))
         ...)]))

(define-pdsd
  [add #x58]
  [div #x5e]
  [max #x5f]
  [min #x5d]
  [mul #x59]
  [sqrt #x51]
  [sub #x5c])

(define-pspd1
  [and #x54]
  [andn #x55]
  [or #x56]
  [xor #x57]
  [unpackl #x14]
  [unpackh #x15])

(define-psss
  [sqrt #x51]
  [rsqrt #x52]
  [rcp #x53]
  [add #x58]
  [mul #x59]
  [sub #x5c]
  [min #x5d]
  [div #x5e]
  [max #x5f])

(define-pspd2 movu
  [(V-Wo) (V-W: #x10)]
  [(Wo-V) (W-V: #x11)])

(define-pspd2 movl
  [(V-Wq) (V-W: #x12)]
  [(Wq-V) (W-V: #x13)])

(define-pspd2 movh
  [(V-Wq) (V-W: #x16)]
  [(Wq-V) (W-V: #x17)])

(define-pspd2 mova
  [(V-Wo) (V-W: #x28)]
  [(Wo-V) (W-V: #x29)])

(define-pspd2 movnt
  [(Mo-V) (W-V: #x2b)])

(define-pspd2 movmsk
  [(Gd-V) (V-W: #x50)])

(define-dispatch movss
  [(V-Wd) (V-W: #x10 #:mandatory-prefix #xf3)]
  [(Wd-V) (W-V: #x11 #:mandatory-prefix #xf3)])
(define-dispatch movsd
  ['() (just #"\xa5")]
  [(V-Wq) (V-W: #x10 #:mandatory-prefix #xf2)]
  [(Wq-V) (W-V: #x11 #:mandatory-prefix #xf2)])


(define-dispatch movhlps
  [(V-V) (V-W: #x12)])

(define-dispatch movlhps
  [(V-V) (V-W: #x16)])
(define-dispatch ucomiss
  [(V-Wd) (V-W: #x2e)])
(define-dispatch comiss
  [(V-Wd) (V-W: #x2f)])
(define-dispatch ucomisd
  [(V-Wd) (V-W: #x2e #:mandatory-prefix #x66)])
(define-dispatch comisd
  [(V-Wd) (V-W: #x2f #:mandatory-prefix #x66)])

(define-dispatch ldmxcsr
  [(Md) (E: '(#x0f #xae) #:/ 2)])

(define-dispatch stmxcsr
  [(Md) (E: '(#x0f #xae) #:/ 3)])

(define-dispatch cmpps
  [(V-Wo-Ib) (V-W-I: #xc2)])
(define-dispatch cmpss
  [(V-Wd-Ib) (V-W-I: #xc2 #:mandatory-prefix #xf3)])
(define-dispatch cmppd
  [(V-Wo-Ib) (V-W-I: #xc2 #:mandatory-prefix #x66)])
(define-dispatch cmpsd
  ['() (just #"\xa7")]
  [(V-Wq-Ib) (V-W-I: #xc2 #:mandatory-prefix #xf2)])

(define-dispatch shufps
  [(V-Wo-Ib) (V-W-I: #xc6)])
(define-dispatch shufpd
  [(V-Wo-Ib) (V-W-I: #xc6 #:mandatory-prefix #x66)])

(define-syntax-rule (define-dispatch* [a b ...] ...)
  (begin
    (define-dispatch a [b ...])
    ...))

(define-dispatch*
  [cvtdq2pd (V-Wq) (V-W: #xe6 #:mandatory-prefix #xf3)]
  [cvtdq2ps (V-Wo) (V-W: #x5b)]
  [cvtpd2dq (V-Wo) (V-W: #xe6 #:mandatory-prefix #xf2)]
  ;cvtpd2pi
  [cvtpd2ps (V-Wo) (V-W: #x5a #:mandatory-prefix #x66)]
  [cvtpi2ps (V-Mq) (V-W: #x2a)];mm
  [cvtpi2pd (V-Mq) (V-W: #x2a #:mandatory-prefix #x66)]
  [cvtps2dq (V-Wo) (V-W: #x5b #:mandatory-prefix #x66)]
  ;CVTPS2PI
  [cvtps2pd (V-Wq) (V-W: #x5a)]
  [cvtsd2si (Gy-Wq) (V-W: #x2d #:mandatory-prefix #xf2)]
  [cvtsd2ss (V-Wq) (V-W: #x5a #:mandatory-prefix #xf2)]
  ;;
  [cvtss2si (Gy-Wd) (V-W: #x2d #:mandatory-prefix #xf3)]
  [cvtss2sd (V-Wd) (V-W: #x5a #:mandatory-prefix #xf3)]
  [cvttpd2dq (V-Wo) (V-W: #xe6 #:mandatory-prefix #x66)]
  [cvttpd2di (V-Wo) (V-W: #x2c #:mandatory-prefix #x66)]
  [cvttps2dq (V-Wo) (V-W: #x5b #:mandatory-prefix #xf3)]
  [cvttsd2si (Gy-Wq) (V-W: #x2c #:mandatory-prefix #xf2)])


(define-dispatch cvtpi2ss
  [(V-Ed) (V-W: #x2a #:mandatory-prefix #xf3)]
  [(V-Eq) (V-W: #x2a #:mandatory-prefix #xf3
                #:override-operand-size (ann 64 Size))])

(define-dispatch cvtsi2sd
  [(V-Ed) (V-W: #x2a #:mandatory-prefix #xf2)]
  [(V-Eq) (V-W: #x2a #:mandatory-prefix #xf2
                #:override-operand-size (ann 64 Size))])

;;; integers

(define-dispatch movd
  [(V-Wd) (V-W: #x6d #:mandatory-prefix #x66)]
  [(Wd-V) (W-V: #x7e #:mandatory-prefix #x66)])
(define-dispatch movq
  [(V-Wq) (V-W: #x7e #:mandatory-prefix #xf3)]
  [(Wq-V) (W-V: #xd6 #:mandatory-prefix #x66)]
  [(Eq-V) (W-V: #x7e #:mandatory-prefix #x66
                #:override-operand-size (ann 64 Size))]
  [(V-Eq) (V-W: #x6e #:mandatory-prefix #x66
                #:override-operand-size (ann 64 Size))])

(define-dispatch*
  [pmovmskb (Gd-V) (V-W: #xd7)]
  [pextrw (Gd-V-Ib)
          (V-W-I: #xc5 #:mandatory-prefix #x66)]
  [pinsrw (V-GdMw-Ib)
          (V-W-I: #xc4 #:mandatory-prefix #x66)])

(define-VW128 #x66
  [packssdw #x6b]
  [packsswb #x63]
  [packuswb #x67]
  [paddb #xfc]
  [paddw #xfd]
  [paddd #xfe]
  [paddq #xd4]
  [paddsb #xec]
  [paddsw #xed]
  [paddusb #xdc]
  [paddusw #xdd]
  [pand #xdb]
  [pandn #xdf]
  [por #xeb]
  [pxor #xef]
  [pcmpeqb #x74]
  [pcmpeqw #x75]
  [pcmpeqd #x76]
  [pcmpgtb #x64]
  [pcmpgtw #x65]
  [pcmpgtd #x66]
  [pmullw #xd5]
  [pmulhw #xe5]
  [pmulhuw #xe4]
  [pmuludq #xf4]
  [psubb #xf8]
  [psubw #xf9]
  [psubd #xfa]
  [psubq #xfb]
  [psubsb #xe8]
  [psubsw #xe9]
  [pmaddws #xf5]
  [psubusb #xd8]
  [psubusw #xd9]
  [punpckhbw #x68]
  [punpckhwd #x69]
  [punpckhdq #x6a]
  [punpcklbw #x60]
  [punpcklwd #x61]
  [punpckldq #x62]
  [pavgb #xe0]
  [pavgw #xe3]
  [pminub #xda]
  [pminsw #xea]
  [pmaxsw #xee]
  [pmaxub #xde]
  [psadbw #xf6])

(define-VW128-VI8 #x66
  [psllw 6 #xf1 #x71]
  [pslld 6 #xf2 #x72]
  [psllq 6 #xf3 #x73]
  [psrad 4 #xe2 #x72]
  [psraw 4 #xe1 #x71]
  [psrlw 2 #xd1 #x71]
  [psrld 2 #xd2 #x72]
  [psrlq 2 #xd3 #x73])

(define-dispatch maskmovdqu
  [(V-V) (V-W: #xf7 #:mandatory-prefix #x66)])

;;movdq2q

(define-dispatch movdqa
  [(V-Wo) (V-W: #x6f #:mandatory-prefix #x66)]
  [(Wo-V) (W-V: #x7f #:mandatory-prefix #x66)])
(define-dispatch movdqu
  [(V-Wo) (V-W: #x6f #:mandatory-prefix #xf3)]
  [(Wo-V) (W-V: #x7f #:mandatory-prefix #xf3)])

;;movq2dq
(define-dispatch movntdq
  [(Mo-V) (W-V: #xe7 #:mandatory-prefix #x66)])

(define-dispatch*
  [pshufhw (V-Wo-Ib)
           (V-W-I: #x70 #:mandatory-prefix #xf3)]
  [pshuflw (V-Wo-Ib)
           (V-W-I: #x70 #:mandatory-prefix #xf2)]
  [pshufd (V-Wo-Ib)
          (V-W-I: #x70 #:mandatory-prefix #x66)])

(define-dispatch*
  [pslldq (V-Ib) (V-I: #x73 #:/ 7 #:mandatory-prefix #x66)]
  [psrldq (V-Ib) (V-I: #x73 #:/ 3 #:mandatory-prefix #x66)])

(define-VW128 #x66
  [punpckhqdq #x6d]
  [punpcklqdq #x6c])

;;; sse3

(define-VW128 #x66
  [addsubpd #xd0]
  [haddpd #x7c]
  [hsubpd #x7d])

(define-VW128 #xf2
  [addsubps #xd0]
  [haddps #x7c]
  [hsubps #x7d])

(define-VW128 #xf3
  [movsldup #x12]
  [movshdup #x16])

(define-dispatch movddup
  [(V-Wq) (V-W: #x12 #:mandatory-prefix #xf2)])

(define-dispatch lddqu
  [(V-Mo) (V-W: #xf0 #:mandatory-prefix #xf2)])

