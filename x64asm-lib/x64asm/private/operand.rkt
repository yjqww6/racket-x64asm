#lang typed/racket/base

(provide (all-defined-out))
(require "registers.rkt" racket/match
         (for-syntax racket/base syntax/parse syntax/name))

(define-type Scale (U 1 2 4 8))

(struct Label ([name : Symbol]
               [assigned? : (Boxof (U Boolean Exact-Nonnegative-Integer))])
  #:transparent)

(define (make-label [name : Symbol])
  (Label name (box #f)))

(struct Imm ([size : Size] [self : (Option Label)]) #:transparent)
(struct Immediate Imm ([num : Integer]) #:transparent)
(struct Relocate Imm () #:transparent)
(struct Relocate:Label Relocate
  ([target : Label] [rel? : Boolean]) #:transparent)
(struct Relocate:Custom Relocate
  ([proc : (-> Integer)])
  #:transparent)
(struct Offset ([size : Size] [num : Imm] [seg : (Option Seg)]) #:transparent)

(struct Mref ([size : Size]
              [base : (U False GPR IP)]
              [index+scale : (Option (Pairof GPR Scale))]
              [disp : (Option Imm)]
              [seg : (Option Seg)]) #:transparent)

(define-type Operand (U Reg Mref Imm Offset))

(define (make-imm [n : Size])
  (λ ([num : (U Integer Label)] #:! [self! : (Option Label) #f])
    (cond
      [(Label? num)
       (define addr (unbox (Label-assigned? num)))
       (if (fixnum? addr)
           (Immediate n self! addr)
           (Relocate:Label n self! num #f))]
      [else (Immediate n self! num)])))

(define imm8 (make-imm (ann 8 Size)))

(define imm16 (make-imm (ann 16 Size)))

(define imm32 (make-imm (ann 32 Size)))

(define imm64 (make-imm (ann 64 Size)))

(define (rel8 [l : Label] #:! [self! : (Option Label) #f])
  (Relocate:Label 8 self! l #t))

(define (rel32 [l : Label] #:! [self! : (Option Label) #f])
  (Relocate:Label 32 self! l #t))

(define (or-imm [num : (U Imm Integer)]) : Imm
  (cond
    [(Imm? num) num]
    [(<= -128 num 127) (Immediate 8 #f num)]
    [else (Immediate 32 #f num)]))

(define (or-imm32 [num : (U Imm Integer)]) : Imm
  (cond
    [(Imm? num) num]
    [else (Immediate 32 #f num)]))

(define (or-imm64 [num : (U Imm Integer)]) : Imm
  (cond
    [(Imm? num) num]
    [else (Immediate 64 #f num)]))

(define (latent-imm [size : Size] [proc : (-> Integer)]
                    #:! [self! : (Option Label) #f])
  (Relocate:Custom size self! proc))

(define (label-addr [l : Label])
  (define a (unbox (Label-assigned? l)))
  (when (boolean? a)
    (error 'label-addr
           "label do not have a address: ~a" l))
  a)
