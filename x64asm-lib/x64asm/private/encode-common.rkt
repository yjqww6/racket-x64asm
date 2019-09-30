#lang typed/racket/base
(require racket/match racket/fixnum
         "assembler.rkt" "registers.rkt" "operand.rkt" "trace.rkt")

(provide (all-defined-out))

(define (mod/rm [reg-bits : Byte] [operand : (U Reg Mref)])
  : (Values Byte Byte (Option Byte) (Option Imm))

  (define b00 0)
  (define b01 #b01000000)
  (define b10 #b10000000)
  (define b11 #b11000000)
  
  (define (disp->mod disp)
    (match disp
      [#f b00]
      [(Imm 8 _) b01]
      [(Imm 32 _) b10]))

  (define (rex.rxb [r : Byte] [x : Byte] [b : Byte]) : Byte
    (bitwise-ior (arithmetic-shift (bitwise-and r #b1000) -1)
                 (bitwise-ior (arithmetic-shift (bitwise-and x #b1000) -2)
                              (arithmetic-shift (bitwise-and b #b1000) -3))))

  (define (modrm/sib [mod : Byte] [reg : Byte] [rm : Byte]) : Byte
    (bitwise-ior mod
                 (bitwise-ior (bitwise-and (arithmetic-shift reg 3) #b111000)
                              (bitwise-and rm #b111))))

  (define (->scale [s : Scale]) : Byte
    (match s
      [1 b00]
      [2 b01]
      [4 b10]
      [8 b11]))
  
  (match operand
    [(?GPR #:code code)
     (values (rex.rxb reg-bits 0 code)
             (modrm/sib b11 reg-bits code)
             #f #f)]
    [(?XMM #:code code)
     (values (rex.rxb reg-bits 0 code)
             (modrm/sib b11 reg-bits code)
             #f #f)]
    [(Mref _ (? IP?) #f (and disp (Imm 32 _)) _)
     (values (rex.rxb reg-bits 0 0)
             (modrm/sib b00 reg-bits #b101)
             #f
             disp)]
    [(Mref _ #f #f (and disp (Imm 32 _)) _)
     (values (rex.rxb reg-bits 0 0)
             (modrm/sib b00 reg-bits #b100)
             (modrm/sib b00 #b100 #b101)
             disp)]
    [(Mref size r #f #f _)
     #:when (eq? r rbp)
     (mod/rm reg-bits (Mref size r #f (Immediate 8 #f 0) #f))]
    [(Mref _ (?GPR #:code code) #f disp _)
     ;#:when (not (= code #b101))
     (values (rex.rxb reg-bits 0 code)
             (modrm/sib (disp->mod disp) reg-bits code)
             #f disp)]
    [(Mref _ #f (cons (?GPR #:code index)  s) disp _)
     #:when (not (eq? index (Reg-code rsp)))
     (values (rex.rxb reg-bits index #b101)
             (modrm/sib b00 reg-bits #b100)
             (modrm/sib (->scale s) index #b101)
             (match disp
               [(Imm 32 _) disp]
               [#f (Immediate 32 #f 0)]
               [(Immediate 8 #f num)
                (Immediate 32 #f num)]
               [else
                (report-error "cannot encode: requires disp32: ~a" disp)]))]
    [(Mref _ (?GPR #:code base) (cons (?GPR #:code index) s) disp _)
     #:when (not (eq? index (Reg-code rsp)))
     (values (rex.rxb reg-bits index base)
             (modrm/sib (disp->mod disp) reg-bits #b100)
             (modrm/sib (->scale s) index base)
             disp)]
    [_
     (report-invalid-operands operand)]))

(define (asm-legacy-prefix! [ctx : Context]
                            [prefix-group-1 : (Option Byte)]
                            [operand-size : Size]
                            [addressing-size : Size]
                            [seg : (Option Seg)]
                            [mandatory-prefix : (Option Byte) #f])
  (when prefix-group-1
    (asm-byte! ctx prefix-group-1))

  (when (= operand-size 16)
    (asm-byte! ctx #x66))

  (when (= addressing-size 32)
    (asm-byte! ctx #x67))

  (when seg
    (asm-byte! ctx (Seg-prefix seg)))

  (when mandatory-prefix
    (asm-byte! ctx mandatory-prefix)))

(define (find-operand-size [G : (U Reg Byte)] [E : (U Reg Mref)]) : Size
  (match* (G E)
    [((?Reg #:size size) _) size]
    [(_ (?Reg #:size size)) size]
    [(_ (Mref size _ _ _ _)) size]))

(define (find-addressing-size [E : (U Reg Mref)]) : Size
  (match E
    [(Mref _ (?IP #:size size) _ _ _) size]
    [(Mref _ (?GPR #:size size) #f _ _) size]
    [(Mref _ #f (cons (?GPR #:size size) _) _ _) size]
    [(Mref _ (?GPR #:size size) (cons (?GPR #:size size) _) _ _) size]
    [_ (ann 64 Size)]))

(define ((encode-common [opcode : Byte]
                        #:prefix-group-1 [prefix-group-1 : (Option Byte) #f]
                        #:mandatory-prefix [mandatory-prefix : (Option Byte) #f]
                        #:opcode-prefix [opcode-prefix : (Option Bytes) #f]
                        #:default-64? [default-64? : Boolean #f]
                        #:override-operand-size [override-operand-size : (Option Size) #f]
                        #:use-second-operand-size? [use-second-operand-size? : Boolean #f])
         [ctx : Context]
         [G : (U Reg Byte)] [E : (U Reg Mref)] [I : (Option Imm)])
  (define operand-size (or override-operand-size (find-operand-size
                                                  (if use-second-operand-size?
                                                      0
                                                      G)
                                                  E)))
  (define addressing-size (find-addressing-size E))
  
  (define (asm-rex! [rxb : Byte])
    (cond
      [(and (= operand-size 64) (not default-64?))
       (asm-byte! ctx (bitwise-ior #b01001000 rxb))]
      [(or (not (fx= rxb 0))
           (and (fx= operand-size 8) (cond
                                       [(Reg? G) (reg-require-rex? G)]
                                       [(Reg? E) (reg-require-rex? E)]
                                       [else #f])))
       (asm-byte! ctx (bitwise-ior #b01000000 rxb))]
      [else (void)]))
  
  (asm-legacy-prefix! ctx prefix-group-1 operand-size
                      addressing-size
                      (and (Mref? E) (Mref-seg E))
                      mandatory-prefix)

  (define code
    (if (Reg? G)
        (Reg-code G)
        G))
  
  (define-values (rxb modrm sib disp)
    (mod/rm code E))
  
  (asm-rex! rxb)
  (when opcode-prefix
    (asm-bytes! ctx opcode-prefix))
  (asm-byte! ctx opcode)
  (asm-byte! ctx modrm)
  (when sib
    (asm-byte! ctx sib))
  (when disp
    (asm-imm! ctx disp))
  (when I
    (asm-imm! ctx I))
  (finish-instruction! ctx))

(define ((encode-common2 [opcode : Byte]
                         #:prefix-group-1 [prefix-group-1 : (Option Byte) #f]
                         #:mandatory-prefix [mandatory-prefix : (Option Byte) #f]
                         #:opcode-prefix [opcode-prefix : (Option Bytes) #f]
                         #:extend-opcode? [extend-opcode? : Boolean #f]
                         #:default-64? [default-64? : Boolean #f])
         [ctx : Context]
         [G : (Option Reg)] [I : (Option Imm)]
         [seg : (Option Seg) #f])
  (define operand-size
    (cond
      [G (Reg-size G)]
      [I (Imm-size I)]
      [else (ann 32 Size)]))
  (define addressing-size (ann 64 Size))
  
  (asm-legacy-prefix! ctx prefix-group-1 operand-size
                      addressing-size seg mandatory-prefix)

  (when G
    (define w (if (and (= operand-size 64) (not default-64?))
                  #b1000
                  #b0000))
    (define b (reg-code-highbit G))
    (define wrxb (bitwise-ior w b))
    
    (cond
      [(not (fx= wrxb 0))
       (asm-byte! ctx (bitwise-ior #b01000000 wrxb))]
      [(and (fx= operand-size 8) (Reg? G) (reg-require-rex? G))
       (asm-byte! ctx (bitwise-ior #b01000000 wrxb))]
      [else (void)]))
  (when opcode-prefix
    (asm-bytes! ctx opcode-prefix))
  (cond
    [extend-opcode?
     (assert G)
     (asm-byte! ctx (bitwise-ior opcode (reg-code-lowbits G)))]
    [else
     (asm-byte! ctx opcode)])
  (when I
    (asm-imm! ctx I))
  (finish-instruction! ctx)
  )