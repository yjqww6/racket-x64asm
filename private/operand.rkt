#lang typed/racket/base

(provide (all-defined-out))
(require "registers.rkt" racket/match
         (for-syntax racket/base syntax/parse syntax/name))

(define-type Scale (U 1 2 4 8))

(struct Label ([name : Symbol] [local? : Boolean] [assigned? : (Boxof Boolean)]) #:transparent)

(define (make-label [name : Symbol] [local? : Boolean #t])
  (Label name local? (box #f)))

(struct Imm ([size : Size]) #:transparent)
(struct Immediate Imm ([num : Integer]) #:transparent)
(struct Relocate Imm ([label : Label] [rel? : Boolean]) #:transparent)

(struct Mref ([size : Size]
              [base : (Option Reg)]
              [index+scale : (Option (Pairof Reg Scale))]
              [disp : (Option Imm)]) #:transparent)

(define (Imm-resize [imm : Imm] [size : Size])
  (match imm
    [(Immediate _ num) (Immediate size num)]
    [(Relocate _ l r) (Relocate size l r)]))

(define-type Operand (U Reg Mref Imm))

(define (make-imm [n : Size])
  (λ ([num : (U Integer Label)])
    (if (Label? num)
        (Relocate n num #f)
        (Immediate n num))))

(define imm8 (make-imm (ann 8 Size)))

(define imm16 (make-imm (ann 16 Size)))

(define imm32 (make-imm (ann 32 Size)))

(define imm64 (make-imm (ann 64 Size)))

(define (rel8 [l : Label])
  (Relocate 8 l #t))

(define (rel32 [l : Label])
  (Relocate 32 l #t))

(define (or-imm [num : (U Imm Integer)]) : Imm
  (cond
    [(Imm? num) num]
    [(<= -128 num 127) (Immediate 8 num)]
    [else (Immediate 32 num)]))

(define (op-resize [op : Operand] [size : Size])
  (match op
    [(Mref s b is d) (Mref size b is d)]
    [(Imm _) (Imm-resize op size)]
    [(?Reg #:size s)
     (unless (= size s)
       (error 'op-resize "unequal size ~a ~a" s size))
     op]))
