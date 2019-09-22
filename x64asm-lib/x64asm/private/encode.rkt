#lang typed/racket/base
(require "assembler.rkt"
         "encode-common.rkt"
         "registers.rkt" "operand.rkt"
         racket/match racket/list)

(provide (all-defined-out))

(define-type Encoder (Context (Listof Operand) -> Void))

(define-syntax-rule (define-encoder (name fmls ...) pat enc args ...)
  (define (name
           [opcode : (U Byte (Pairof Byte (Listof Byte)))]
           fmls ...
           #:prefix-group-1 [prefix-group-1 : (Option Byte) #f]
           #:mandatory-prefix [mandatory-prefix : (Option Byte) #f]
           #:default-64? [default-64? : Boolean #f]) : Encoder
    (: code Byte)
    (: prefix (Option Bytes))
    (define-values (code prefix)
      (cond
        [(byte? opcode)
         (values opcode #f)]
        [else (values (last opcode) (list->bytes (drop-right opcode 1)))]))
    (λ ([ctx : Context]
        [operands : (Listof Operand)])
      (match-define pat operands)
      (enc ctx code args ...
           #:prefix-group-1 prefix-group-1
           #:mandatory-prefix mandatory-prefix
           #:opcode-prefix prefix
           #:default-64? default-64?))))

(define-encoder (G-E: #:override-operand-size [override-operand-size : (Option Size) #f])
  (list (? Reg? a) (or (? Reg? b) (? Mref? b)))
  encode-common a b #f #:override-operand-size override-operand-size)

(define-encoder (G-E-I:)
  (list (? Reg? a) (or (? Reg? b) (? Mref? b)) (? Imm? c))
  encode-common a b c)

(define-encoder (E-G-I:)
  (list (or (? Reg? b) (? Mref? b)) (? Reg? a) (? Imm? c))
  encode-common a b c)

(define-encoder (E-G: #:override-operand-size [override-operand-size : (Option Size) #f])
  (list (or (? Reg? b) (? Mref? b)) (? Reg? a))
  encode-common a b #f #:override-operand-size override-operand-size)

(define-encoder (E-S:)
  (list (and (or (?Reg #:size size) (Mref size _ _ _ _))
             b)
        (? Seg? a))
  encode-common a b #f #:override-operand-size size)

(define-encoder (E-G_:)
  (list (or (? Reg? b) (? Mref? b)) (? Reg? a) _)
  encode-common a b #f)

(define-encoder (E-I: #:/ [reg-field : Byte 0])
  (list (or (? Reg? a) (? Mref? a)) (? Imm? b))
  encode-common reg-field a b)

(define-encoder (E: #:/ [reg-field : Byte 0]
                    #:override-operand-size [override-operand-size : (Option Size) #f])
  (list (or (? Reg? b) (? Mref? b)))
  encode-common reg-field b #f #:override-operand-size override-operand-size)

(define-encoder (E_: #:/ [reg-field : Byte 0])
  (list (or (? Reg? b) (? Mref? b)) _)
  encode-common reg-field b #f)

(define-encoder (_E: #:/ [reg-field : Byte 0])
  (list _ (or (? Reg? b) (? Mref? b)))
  encode-common reg-field b #f)

(define-encoder (G-I: #:extend-opcode? [extend-opcode? : Boolean #f])
  (list (? Reg? a) (? Imm? b))
  encode-common2 a b #:extend-opcode? extend-opcode?)

(define-encoder (I-G: #:extend-opcode? [extend-opcode? : Boolean #f])
  (list (? Imm? b) (? Reg? a))
  encode-common2 a b #:extend-opcode? extend-opcode?)

(define-encoder (G-O: #:extend-opcode? [extend-opcode? : Boolean #f])
  (list (? Reg? a) (Offset _ num seg))
  encode-common2 a num #:extend-opcode? extend-opcode? #:seg seg)

(define-encoder (O-G: #:extend-opcode? [extend-opcode? : Boolean #f])
  (list (Offset _ num seg) (? Reg? a))
  encode-common2 a num #:extend-opcode? extend-opcode? #:seg seg)

(define-encoder (G: #:extend-opcode? [extend-opcode? : Boolean #f])
  (list (? Reg? a))
  encode-common2 a #f #:extend-opcode? extend-opcode?)

(define-encoder (G_: #:extend-opcode? [extend-opcode? : Boolean #f])
  (list (? Reg? a) _)
  encode-common2 a #f #:extend-opcode? extend-opcode?)

(define-encoder (_G: #:extend-opcode? [extend-opcode? : Boolean #f])
  (list _ (? Reg? a))
  encode-common2 a #f #:extend-opcode? extend-opcode?)

(define-encoder (I:)
  (list (? Imm? a))
  encode-common2 #f a)

(define-encoder (J: #:extend-opcode? [extend-opcode? : Boolean #f])
  (list (? Imm? a))
  encode-common2 #f a #:extend-opcode? extend-opcode?)

(define (just [b : Bytes])
  (λ ([ctx : Context]
      [operands : (Listof Operand)])
    (asm-bytes! ctx b)
    (finish-instruction! ctx)))

(define (I*: [opcode : Byte])
  (λ ([ctx : Context]
      [ops : (Listof Operand)])
    (asm-byte! ctx opcode)
    (when (andmap Imm? ops)
      (for ([i (in-list ops)])
        (asm-imm! ctx i)))
    (finish-instruction! ctx)))