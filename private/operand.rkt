#lang typed/racket/base

(provide (except-out (all-defined-out)
                     with-labels-helper
                     current-labels-target))
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

(define-syntax (mref stx)
  (syntax-parse stx #:literals (+ - *)
    [(_ size a + b * c)
     #'(Mref size a (cons b (ann c Scale)) #f)]
    [(_ size a + b * c + d)
     #'(Mref size a (cons b (ann c Scale)) (or-imm d))]
    [(_ size a + b * c - d)
     #'(Mref size a (cons b (ann c Scale)) (or-imm (- d)))]
    [(_ size b * c + d)
     #'(Mref size #f (cons b (ann c Scale)) (or-imm d))]
    [(_ size b * c - d)
     #'(Mref size #f (cons b (ann c Scale)) (or-imm (- d)))]
    [(_ size a + d:number)
     #'(Mref size a #f (or-imm d))]
    [(_ size a - d:number)
     #'(Mref size a #f (or-imm (- d)))]
    [(_ size a + d)
     #'(let ([ts : Size size]
             [ta a]
             [td d])
         (if (Reg? td)
             (Mref ts ta (cons td (ann 1 Scale)) #f)
             (Mref ts ta #f (or-imm td))))]
    [(_ size b * c)
     #'(Mref size #f (cons b (ann c Scale)) #f)]
    [(_ size a)
     #'(Mref size a #f #f)]))

(define-syntax-rule (moff s num)
  (Mref (ann s Size) #f #f (Immediate 64 num)))


(require racket/stxparam)

(define-syntax (label stx)
  (syntax-parse stx
    [(_)
     #`(make-label '#,(syntax-local-infer-name stx) #t)]
    [(_ a)
     (define table (syntax-parameter-value #'current-labels-target))
     (hash-ref! table (syntax-e #'a)
                (λ () (syntax-local-lift-expression #'(make-label 'a #t))))]))

(define-syntax (entry stx)
  (syntax-parse stx
    [(_)
     #`(make-label '#,(syntax-local-infer-name stx) #f)]
    [(_ a)
     (define table (syntax-parameter-value #'current-labels-target))
     (hash-ref! table (syntax-e #'a)
                (λ () (syntax-local-lift-expression #'(make-label 'a #f))))]))

(define (op-resize [op : Operand] [size : Size])
  (match op
    [(Mref s b is d) (Mref size b is d)]
    [(Imm _) (Imm-resize op size)]
    [(?Reg #:size s)
     (unless (= size s)
       (error 'op-resize "unequal size ~a ~a" s size))
     op]))

(define-syntax-parameter current-labels-target #f)

(define-syntax (with-labels-helper stx)
  (syntax-parse stx
    [(_ body ...)
     (local-expand/capture-lifts
      #'(let () body ...)
      'expression
      '())]))

(define-syntax (with-labels stx)
  (syntax-parse stx
    [(_ ((~alt (~seq #:entry e:id) l:id) ...) body ...)
     #`(let ()
         (define e (entry)) ...
         (define l (label)) ...
         (syntax-parameterize
             ([current-labels-target
               (make-hasheq
                (list (cons 'l #'l) ...
                      (cons 'e #'e) ...))])
           (with-labels-helper body ...))
         )
     ]))