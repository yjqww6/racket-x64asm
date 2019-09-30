#lang typed/racket/base
(require "assembler.rkt"
         "encode-common.rkt"
         "registers.rkt" "operand.rkt"
         racket/match racket/list)

(provide (all-defined-out))

(define-syntax-rule (define-encoder (name fmls ...) (pat ...) (enc eargs ...) args ...)
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
    (define encoder
      (enc code
           #:prefix-group-1 prefix-group-1
           #:mandatory-prefix mandatory-prefix
           #:opcode-prefix prefix
           #:default-64? default-64?
           eargs ...))
    (λ ([ctx : Context] pat ...)
      (encoder ctx args ...))))

(define-encoder (G-E: #:override-operand-size [override-operand-size : (Option Size) #f])
  ([a : Reg] [b : (U Reg Mref)])
  (encode-common #:override-operand-size override-operand-size) a b #f)

(define-encoder (G-E-I:)
  ([a : Reg] [b : (U Reg Mref)] [c : Imm])
  (encode-common) a b c)

(define-encoder (E-G-I:)
  ([b : (U Reg Mref)] [a : Reg] [c : Imm])
  (encode-common) a b c)

(define-encoder (E-G: #:override-operand-size [override-operand-size : (Option Size) #f])
  ([b : (U Reg Mref)] [a : Reg])
  (encode-common #:override-operand-size override-operand-size) a b #f)

(define-encoder (G-S:)
  ([b : Reg] [a : Seg])
  (encode-common #:use-second-operand-size? #t) a b #f)

(define-encoder (E-G_:)
  ([b : (U Reg Mref)] [a : Reg] [_ : Any])
  (encode-common) a b #f)

(define-encoder (E-I: #:/ [reg-field : Byte 0])
  ([a : (U Reg Mref)] [b : Imm])
  (encode-common) reg-field a b)

(define-encoder (E: #:/ [reg-field : Byte 0]
                    #:override-operand-size [override-operand-size : (Option Size) #f])
  ([b : (U Reg Mref)])
  (encode-common #:override-operand-size override-operand-size) reg-field b #f)

(define-encoder (E_: #:/ [reg-field : Byte 0])
  ([b : (U Reg Mref)] [_ : Any])
  (encode-common) reg-field b #f)

(define-encoder (_E: #:/ [reg-field : Byte 0])
  ([_ : Any] [b : (U Reg Mref)])
  (encode-common) reg-field b #f)

(define-encoder (G-I: #:extend-opcode? [extend-opcode? : Boolean #f])
  ([a : Reg] [b : Imm])
  (encode-common2 #:extend-opcode? extend-opcode?) a b)

(define-encoder (I-G: #:extend-opcode? [extend-opcode? : Boolean #f])
  ([b : Imm] [a : Reg])
  (encode-common2 #:extend-opcode? extend-opcode?) a b)

(define-encoder (G-O: #:extend-opcode? [extend-opcode? : Boolean #f])
  ([a : Reg] [b : Offset])
  (encode-common2 #:extend-opcode? extend-opcode?) a (Offset-num b) (Offset-seg b))

(define-encoder (O-G: #:extend-opcode? [extend-opcode? : Boolean #f])
  ([b : Offset] [a : Reg])
  (encode-common2 #:extend-opcode? extend-opcode?) a (Offset-num b) (Offset-seg b))

(define-encoder (G: #:extend-opcode? [extend-opcode? : Boolean #f])
  ([a : Reg])
  (encode-common2 #:extend-opcode? extend-opcode?) a #f)

(define-encoder (G_: #:extend-opcode? [extend-opcode? : Boolean #f])
  ([a : Reg] [_ : Any])
  (encode-common2 #:extend-opcode? extend-opcode?) a #f)

(define-encoder (_G: #:extend-opcode? [extend-opcode? : Boolean #f])
  ([_ : Any] [a : Reg])
  (encode-common2 #:extend-opcode? extend-opcode?) a #f)

(define-encoder (I:)
  ([a : Imm])
  (encode-common2)  #f a)

(define-encoder (J: #:extend-opcode? [extend-opcode? : Boolean #f])
  ([a : Imm])
  (encode-common2 #:extend-opcode? extend-opcode?) #f a)

(define (just [b : Bytes])
  (λ ([ctx : Context])
    (asm-bytes! ctx b)
    (finish-instruction! ctx)))

(define (just1 [b : Bytes])
  (λ ([ctx : Context] [_ : Operand])
    (asm-bytes! ctx b)
    (finish-instruction! ctx)))

(define (I-I: [opcode : Byte])
  (λ ([ctx : Context] [a : Imm] [b : Imm])
    (asm-byte! ctx opcode)
    (asm-imm! ctx a)
    (asm-imm! ctx b)
    (finish-instruction! ctx)))