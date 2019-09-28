#lang typed/racket/base
(require "assembler.rkt"
         "encode-common.rkt"
         "registers.rkt" "operand.rkt"
         racket/match racket/list)

(provide (all-defined-out))

(define-syntax-rule (define-encoder (name fmls ...) (pat ...) enc args ...)
  (define (name
           [opcode : (U Byte (Pairof Byte (Listof Byte)))]
           fmls ...
           #:prefix-group-1 [prefix-group-1 : (Option Byte) #f]
           #:mandatory-prefix [mandatory-prefix : (Option Byte) #f]
           #:default-64? [default-64? : Boolean #f])
    (: code Byte)
    (: prefix (Option Bytes))
    (define-values (code prefix)
      (cond
        [(byte? opcode)
         (values opcode #f)]
        [else (values (last opcode) (list->bytes (drop-right opcode 1)))]))
    (λ ([ctx : Context] pat ...)
      (enc ctx code args ...
           #:prefix-group-1 prefix-group-1
           #:mandatory-prefix mandatory-prefix
           #:opcode-prefix prefix
           #:default-64? default-64?))))

(define-encoder (G-E: #:override-operand-size [override-operand-size : (Option Size) #f])
  ([a : Reg] [b : (U Reg Mref)])
  encode-common a b #f #:override-operand-size override-operand-size)

(define-encoder (G-E-I:)
  ([a : Reg] [b : (U Reg Mref)] [c : Imm])
  encode-common a b c)

(define-encoder (E-G-I:)
  ([b : (U Reg Mref)] [a : Reg] [c : Imm])
  encode-common a b c)

(define-encoder (E-G: #:override-operand-size [override-operand-size : (Option Size) #f])
  ([b : (U Reg Mref)] [a : Reg])
  encode-common a b #f #:override-operand-size override-operand-size)

(define-encoder (E-S:)
  ([b : (U Reg Mref)] [a : Seg])
  encode-common a b #f #:override-operand-size (if (Reg? b)
                                                   (Reg-size b)
                                                   (Mref-size b)))

(define-encoder (E-G_:)
  ([b : (U Reg Mref)] [a : Reg] [_ : Any])
  encode-common a b #f)

(define-encoder (E-I: #:/ [reg-field : Byte 0])
  ([a : (U Reg Mref)] [b : Imm])
  encode-common reg-field a b)

(define-encoder (E: #:/ [reg-field : Byte 0]
                    #:override-operand-size [override-operand-size : (Option Size) #f])
  ([b : (U Reg Mref)])
  encode-common reg-field b #f #:override-operand-size override-operand-size)

(define-encoder (E_: #:/ [reg-field : Byte 0])
  ([b : (U Reg Mref)] [_ : Any])
  encode-common reg-field b #f)

(define-encoder (_E: #:/ [reg-field : Byte 0])
  ([_ : Any] [b : (U Reg Mref)])
  encode-common reg-field b #f)

(define-encoder (G-I: #:extend-opcode? [extend-opcode? : Boolean #f])
  ([a : Reg] [b : Imm])
  encode-common2 a b #:extend-opcode? extend-opcode?)

(define-encoder (I-G: #:extend-opcode? [extend-opcode? : Boolean #f])
  ([b : Imm] [a : Reg])
  encode-common2 a b #:extend-opcode? extend-opcode?)

(define-encoder (G-O: #:extend-opcode? [extend-opcode? : Boolean #f])
  ([a : Reg] [b : Offset])
  encode-common2 a (Offset-num b) #:extend-opcode? extend-opcode? #:seg (Offset-seg b))

(define-encoder (O-G: #:extend-opcode? [extend-opcode? : Boolean #f])
  ([b : Offset] [a : Reg])
  encode-common2 a (Offset-num b) #:extend-opcode? extend-opcode? #:seg (Offset-seg b))

(define-encoder (G: #:extend-opcode? [extend-opcode? : Boolean #f])
  ([a : Reg])
  encode-common2 a #f #:extend-opcode? extend-opcode?)

(define-encoder (G_: #:extend-opcode? [extend-opcode? : Boolean #f])
  ([a : Reg] [_ : Any])
  encode-common2 a #f #:extend-opcode? extend-opcode?)

(define-encoder (_G: #:extend-opcode? [extend-opcode? : Boolean #f])
  ([_ : Any] [a : Reg])
  encode-common2 a #f #:extend-opcode? extend-opcode?)

(define-encoder (I:)
  ([a : Imm])
  encode-common2 #f a)

(define-encoder (J: #:extend-opcode? [extend-opcode? : Boolean #f])
  ([a : Imm])
  encode-common2 #f a #:extend-opcode? extend-opcode?)

(define (just [b : Bytes])
  (λ ([ctx : Context]
      operands : Operand *)
    (asm-bytes! ctx b)
    (finish-instruction! ctx)))

(define (I*: [opcode : Byte])
  (λ ([ctx : Context]
      ops : Imm *)
    (asm-byte! ctx opcode)
    (for ([i (in-list ops)])
      (asm-imm! ctx i))
    (finish-instruction! ctx)))