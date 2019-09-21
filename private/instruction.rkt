#lang typed/racket/base
(require "encode.rkt" "cases2.rkt" "assembler.rkt" "dispatch.rkt"
         "registers.rkt" "operand.rkt"
         racket/match
         (for-syntax racket/base syntax/parse syntax/name))

(define-for-syntax (generate-indices stx)
  (datum->syntax #'k (build-list (length (syntax->list stx)) values)))

(define-syntax (define-group-1 stx)
  (syntax-parse stx
    [(_) #'(begin)]
    [(_ [name:id s:number field:number] r ...)
     (define start (syntax-e #'s))
     #`(begin
         (define-dispatch name
           [(AL-Ib) (G-I: #,(+ start #x04))]
           [(rAX-Iz) (G-I: #,(+ start #x05))]
           [(Eb-Ib) (E-I: #x80 #:/ field)]
           [(Ev-Iz) (E-I: #x81 #:/ field)]
           [(Ev-Ib) (E-I: #x83 #:/ field)]
           [(Eb-Gb) (E-G: #,(+ start #x00))]
           [(Ev-Gv) (E-G: #,(+ start #x01))]
           [(Gb-Eb) (G-E: #,(+ start #x02))]
           [(Gv-Ev) (G-E: #,(+ start #x03))])
         (define-group-1 r ...))]))

(define-syntax (define-group-2 stx)
  (syntax-parse stx
    [(_ name ...)
     #:with (num ...) (generate-indices #'(name ...))
     #'(begin
         (define-dispatch name
           [(Eb-1) (E_: #xd0 #:/ num)]
           [(Ev-1) (E_: #xd1 #:/ num)]
           [(Eb-CL) (E_: #xd2 #:/ num)]
           [(Ev-CL) (E_: #xd3 #:/ num)]
           [(Eb-Ib) (E-I: #xc0 #:/ num)]
           [(Ev-Ib) (E-I: #xc1 #:/ num)])
         ...)]))

(define-syntax (define-Jcc stx)
  (syntax-parse stx
    [(_ name ...)
     #:with (num ...) (generate-indices #'(name ...))
     #'(begin
         (define-dispatch name
           [(Jb) (J: (cast (+ #x70 num) Byte))]
           [(Jd) (J: (list #x0f (cast (+ #x80 num) Byte)))])
         ...)]))

(define-syntax (define-cmovcc stx)
  (syntax-parse stx
    [(_ name ...)
     #:with (num ...) (generate-indices #'(name ...))
     #'(begin
         (define-dispatch name
           [(Gv-Ev) (G-E: (list #x0f (cast (+ #x40 num) Byte)))])
         ...)]))

(define-syntax (define-setcc stx)
  (syntax-parse stx
    [(_ name ...)
     #:with (num ...) (generate-indices #'(name ...))
     #'(begin
         (define-dispatch name
           [(Eb) (E: (list #x0f (cast (+ #x90 num) Byte)))])
         ...)]))

;;treat them as instructions ...
(define-syntax-rule (define-prefix b prefix ...)
  (begin
    (~@
     (define-syntax (prefix stx)
       (syntax-parse stx
         [(_ (~alt (~optional (~seq #:ctx ctx) #:defaults ([ctx #'(assert (current-context))]))
                   others)
             (... ...))
          #'(let ([c ctx])
              (asm-byte! c b)
              (others (... ...) #:ctx c))]))
     (provide prefix))
    ...))
(define-prefix #xf0 lock)
(define-prefix #xf2 repne repnz)
(define-prefix #xf3 rep repe repz)

(define-group-1
  [add #x00 0]
  [adc #x10 2]
  [and #x20 4]
  [xor #x30 6]
  [or  #x08 1]
  [sbb #x18 3]
  [sub #x28 5]
  [cmp #x38 7])

(define-group-2
  rol ror rcl rcr (shl sal) shr () sar)

(define-dispatch test
  [(AL-Ib) (I: #xa8)]
  [(rAX-Iz) (I: #xa9)]
  [(Eb-Ib) (E-I: #xf6)]
  [(Ev-Iz) (E-I: #xf7)]
  [(Eb-Gb) (E-G: #x84)]
  [(Ev-Gv) (E-G: #x85)])

(define-dispatch not
  [(Eb) (E: #xf6 #:/ 2)]
  [(Ev) (E: #xf7 #:/ 2)])

(define-dispatch neg
  [(Eb) (E: #xf6 #:/ 3)]
  [(Ev) (E: #xf7 #:/ 3)])

(define-dispatch mul
  [(Eb) (E: #xf6 #:/ 4)]
  [(Ev) (E: #xf7 #:/ 4)])

(define-dispatch imul
  [(Eb) (E: #xf6 #:/ 5)]
  [(Ev) (E: #xf7 #:/ 5)]
  [(Gv-Ev) (G-E: '(#x0f #xaf))]
  [(Gv-Ev-Ib) (G-E-I: #x6b)]
  [(Gv-Ev-Iz) (G-E-I: #x69)])

(define-dispatch div
  [(Eb) (E: #xf6 #:/ 6)]
  [(Ev) (E: #xf7 #:/ 6)])

(define-dispatch idiv
  [(Eb) (E: #xf6 #:/ 7)]
  [(Ev) (E: #xf7 #:/ 7)])

(define-dispatch inc
  [(Eb) (E: #xfe)]
  [(Ev) (E: #xff)])

(define-dispatch dec
  [(Eb) (E: #xfe #:/ 1)]
  [(Ev) (E: #xff #:/ 1)])

(define-dispatch call
  [(Jd) (J: #xe8)]
  [(Eq) (E: #xff #:/ 2 #:default-64? #t)])

(define-dispatch jmp
  [(Jb) (J: #xeb)]
  [(Jd) (J: #xe9)]
  [(Eq) (E: #xff #:/ 4 #:default-64? #t)])

(define-dispatch ret
  ['() (just #"\xc3")]
  [(Iw) (I: #xc2)])

(define-dispatch push
  [(Gx) (G: #x50 #:extend-opcode? #t #:default-64? #t)]
  [(Ex) (E: #xff #:/ 6 #:default-64? #t)]
  [(Ib) (I: #x6a)]
  [(Iz) (I: #x68)]
  [(FS-) (just #"\x0f\xa0")]
  [(GS-) (just #"\x0f\xa8")])

(define-dispatch pop
  [(Gx) (G: #x58 #:extend-opcode? #t #:default-64? #t)]
  [(Ex) (E: #x8f #:/ 0 #:default-64? #t)]
  [(FS-) (just #"\x0f\xa1")]
  [(GS-) (just #"\x0f\xa9")])

(define-Jcc
  jo jno
  (jb jc jnae) (jnb jnc jae)
  (jz je) (jnz jne)
  (jbe jna) (jnbe ja)
  js jns
  (jp jpe) (jnp jpo)
  (jl jnge) (jnl jge)
  (jle jng) (jnle jg))

(define-dispatch mov
  [(Eb-Gb) (E-G: #x88)]
  [(Ev-Gv) (E-G: #x89)]
  [(Gb-Eb) (G-E: #x8a)]
  [(Gv-Ev) (G-E: #x8b)]
  [(Gv-S) (E-S: #x8c)]
  [(Mw-S) (E-G: #x8c #:override-operand-size (ann 32 Size))]
  [(S-Ew) (G-E: #x8e #:override-operand-size (ann 32 Size))]
  [(AL-Ob) (G-O: #xa0)]
  [(rAX-Ov) (G-O: #xa1)]
  [(AL-Gb) (O-G: #xa2)]
  [(rAX-Gv) (O-G: #xa3)]
  [(Gb-Ib) (G-I: #xb0 #:extend-opcode? #t)]
  [(Gv-Iv) (G-I: #xb8 #:extend-opcode? #t)]
  [(Eb-Ib) (E-I: #xc6)]
  [(Ev-Iz) (E-I: #xc7)])

(define-dispatch movsb
  ['() (just #"\xa4")])
(define-dispatch movsw
  ['() (just #"\x66\xa5")])
#;
(define-dispatch movsd
  ['() (just #"\xa5")])
(define-dispatch movsq
  ['() (just #"\x48\xa5")])

(define-cmovcc
  cmovo cmovno (cmovb cmovc cmovnae) (cmovnb cmovnc cmovae)
  (cmovz cmove) (cmovnz cmovne) (cmovbe cmovna) (cmovnbe cmova)
  cmovs cmovns (cmovp cmovpe) (cmovnp cmovpo)
  (cmovl cmovnge) (cmovnl cmovge) (cmovle cmovng) (cmovnle cmovg))

(define-dispatch adcx
  [(Gy-Ey) (G-E: '(#x0f #x38 #xf6) #:mandatory-prefix #x66)])


(define-dispatch adox
  [(Gy-Ey) (G-E: '(#x0f #x38 #xf6) #:mandatory-prefix #xf3)])


(define-dispatch bsf
  [(Gv-Ev) (G-E: '(#x0f #xbc))])

(define-dispatch bsr
  [(Gv-Ev) (G-E: '(#x0f #xbd))])

(define-dispatch bswap
  [(Gy) (G: '(#x0f #xc8) #:extend-opcode? #t)])

(define-dispatch bt
  [(Ev-Gv) (E-G: '(#x0f #xa3))]
  [(Ev-Ib) (E-I: '(#x0f #xba) #:/ 4)])

(define-dispatch btc
  [(Ev-Gv) (E-G: '(#x0f #xbb))]
  [(Ev-Ib) (E-I: '(#x0f #xba) #:/ 7)])

(define-dispatch btr
  [(Ev-Gv) (E-G: '(#x0f #xb3))]
  [(Ev-Ib) (E-I: '(#x0f #xba) #:/ 6)])

(define-dispatch bts
  [(Ev-Gv) (E-G: '(#x0f #xab))]
  [(Ev-Ib) (E-I: '(#x0f #xba) #:/ 5)])

(define-dispatch cbw
  ['() (just #"\x66\x98")])

(define-dispatch cwde
  ['() (just #"\x98")])

(define-dispatch cwqe
  ['() (just #"\x48\x98")])

(define-dispatch cwd
  ['() (just #"\x66\x99")])

(define-dispatch cdq
  ['() (just #"\x99")])

(define-dispatch cdo
  ['() (just #"\x48\x99")])

(define-dispatch clc
  ['() (just #"\xf8")])

(define-dispatch cld
  ['() (just #"\xfc")])

(define-dispatch clflush
  [(Mb) (E: '(#x0f #xae) #:/ 7)])

(define-dispatch clflushopt
  [(Mb) (E: '(#x0f #xae) #:/ 7 #:mandatory-prefix #x66)])

(define-dispatch clwb
  [(Mb) (E: '(#x0f #xae) #:/ 6 #:mandatory-prefix #x66)])

(define-dispatch cmc
  ['() (just #"\xf5")])

(define-dispatch cmpsb
  ['() (just #"\xa6")])
(define-dispatch cmpsw
  ['() (just #"\x66\xa7")])
#;
(define-dispatch cmpsd
  ['() (just #"\xa7")])
(define-dispatch cmpsq
  ['() (just #"\x48\xa7")])

(define-dispatch cmpxchg
  [(Eb-Gb) (E-G: '(#x0f #xb0))]
  [(Ev-Gv) (E-G: '(#x0f #xb1))])

(define-dispatch cmpxchg8b
  [(Mq) (E: '(#x0f #xc7) #:/ 1 #:override-operand-size (ann 32 Size))])
(define-dispatch cmpxchg16b
  [(Mo) (E: '(#x0f #xc7) #:/ 1 #:override-operand-size (ann 64 Size))])

(define-dispatch cpuid
  ['() (just #"\x0f\xa2")])

(define-dispatch crc32
  [(Gy-Eb) (G-E: '(#x0f #x38 #xf0) #:mandatory-prefix #xf2)]
  [(Gd-Ew) (G-E: '(#x0f #x38 #xf1) #:mandatory-prefix #xf2
                              #:override-operand-size (ann 16 Size))]
  [(Gy-Ey) (G-E: '(#x0f #x38 #xf1) #:mandatory-prefix #xf2)])

(define-dispatch enter
  [(Iw-Ib) (I*: #xc8)])

(define-dispatch in
  [(AL-Ib) (G-I: #xe4)]
  [(eAX-Ib) (G-I: #xe5)]
  [(AL-DX) (G_: #xec)]
  [(eAX-DX) (G_: #xed)])

(define-dispatch insb
  ['() (just #"\x6c")])
(define-dispatch insw
  ['() (just #"\x66\x6d")])
(define-dispatch insd
  ['() (just #"\x6d")])

(define-dispatch int
  [(Ib) (I: #xcd)])
(define-dispatch into
  ['() (just #"\xce")])

(define-dispatch jrcxz
  [(Jb) (J: #xe3)])

(define-dispatch lahf
  ['() (just #"\x9f")])

(define-dispatch lea
  [(Gv-Mv) (G-E: #x8d)])

(define-dispatch leave
  ['() (just #"\xc9")])

(define-dispatch lfence
  ['() (just #"\x0f\xae\xe8")])

(define-dispatch lodsb
  ['() (just #"\xac")])
(define-dispatch lodsw
  ['() (just #"\x66\xad")])
(define-dispatch lodsd
  ['() (just #"\xad")])
(define-dispatch lodsq
  ['() (just #"\x48\xad")])

(define-dispatch loop
  [(Jb) (J: #xe2)])

(define-dispatch (loope loopz)
  [(Jb) (J: #xe1)])

(define-dispatch (loopne loopnz)
  [(Jb) (J: #xe0)])

(define-dispatch lzcnt
  [(Gv-Ev) (G-E: '(#x0f #xbd) #:mandatory-prefix #xf3)])

(define-dispatch mfence
  ['() (just #"\x0f\xae\xf0")])

(define-dispatch movbe
  [(Gv-Ev) (G-E: '(#x0f #x38 #xf0))]
  [(Ev-Gv) (E-G: '(#x0f #x38 #xf1))])

(define-dispatch movsx
  [(Gv-Eb) (G-E: '(#x0f #xbe))]
  [(Gy-Ew) (G-E: '(#x0f #xbf))])

(define-dispatch movsxd
  [(Gq-Ed) (G-E: #x63)])

(define-dispatch movzx
  [(Gv-Eb) (G-E: '(#x0f #xb6))]
  [(Gy-Ew) (G-E: '(#x0f #xb7))])

(define-dispatch nop
  ['() (just #"\x90")]
  [(Ev) (E: '(#x0f #x1f) #:/ 0)])

(define-dispatch out
  [(Ib-AL) (I-G: #xe6)]
  [(Ib-eAX) (I-G: #xe7)]
  [(DX-AL) (_G: #xee)]
  [(DX-eAX) (_G: #xef)])

(define-dispatch outsb
  ['() (just #"\x6e")])
(define-dispatch outsw
  ['() (just #"\x66\x6f")])
(define-dispatch outsd
  ['() (just #"\x6f")])

(define-dispatch pause
  ['() (just #"\xf3\x90")])

(define-dispatch popcnt
  [(Gv-Ev) (G-E: '(#x0f #xb8) #:mandatory-prefix #xf3)])

(define-dispatch popf
  ['() (just #"\x66\x9d")])
(define-dispatch popfq
  ['() (just #"\x9d")])

(define-dispatch prefetch
  [(Mb) (E: '(#x0f #x0d) #:/ 0)])
(define-dispatch prefetchw
  [(Mb) (E: '(#x0f #x0d) #:/ 1)])
(define-dispatch prefetchnta
  [(Mb) (E: '(#x0f #x18) #:/ 0)])
(define-dispatch prefetcht0
  [(Mb) (E: '(#x0f #x18) #:/ 1)])
(define-dispatch prefetcht1
  [(Mb) (E: '(#x0f #x18) #:/ 2)])
(define-dispatch prefetcht2
  [(Mb) (E: '(#x0f #x18) #:/ 3)])

(define-dispatch pushf
  ['() (just #"\x66\x9c")])
(define-dispatch pushfq
  ['() (just #"\x9c")])

(define-dispatch rdrand
  [(Gv) (E: '(#x0f #xc7) #:/ 6)])
(define-dispatch rdseed
  [(Gv) (E: '(#x0f #xc7) #:/ 7)])

(define-dispatch scasb
  ['() (just #"\xae")])
(define-dispatch scasw
  ['() (just #"\x66\xaf")])
(define-dispatch scasd
  ['() (just #"\xaf")])
(define-dispatch scasq
  ['() (just #"\x48\xaf")])

(define-setcc
  seto setno (setb setc setnae) (setnb setnc setae)
  (setz sete) (setnz setne) (setbe setna) (setnbe seta)
  sets setns (setp setpe) (setnp setpo)
  (setl setnge) (setnl setge) (setle setng) (setnle setg))

(define-dispatch sfence
  ['() (just #"\x0f\xae\xf8")])

(define-dispatch shld
  [(Ev-Gv-Ib) (E-G-I: '(#x0f #xa4))]
  [(Ev-Gv-CL) (E-G_: '(#x0f #xa5))])
(define-dispatch shrd
  [(Ev-Gv-Ib) (E-G-I: '(#x0f #xac))]
  [(Ev-Gv-CL) (E-G_: '(#x0f #xad))])

(define-dispatch stc
  ['() (just #"\xf9")])

(define-dispatch std
  ['() (just #"\xfd")])

(define-dispatch stosb
  ['() (just #"\xaa")])
(define-dispatch stosw
  ['() (just #"\x66\xab")])
(define-dispatch stosd
  ['() (just #"\xab")])
(define-dispatch stosq
  ['() (just #"\x48\xab")])

(define-dispatch tzcnt
  [(Gv-Ev) (G-E: '(#x0f #xbc) #:mandatory-prefix #xf3)])

(define-dispatch xadd
  [(Eb-Gb) (E-G: '(#x0f #xc0))]
  [(Ev-Gv) (E-G: '(#x0f #xc1))])

(define-dispatch xchg
  [(rAX-Gv) (_G: #x90)]
  [(Gv-rAX) (G_: #x90)]
  [(Gb-Eb) (G-E: #x86)]
  [(Eb-Gb) (E-G: #x86)]
  [(Gv-Ev) (G-E: #x86)]
  [(Ev-Gv) (E-G: #x86)])

(define-dispatch xlatb
  ['() (just #"\xd7")])
