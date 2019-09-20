#lang typed/racket/base

(require "operand.rkt" "registers.rkt"
         racket/match
         (for-syntax racket/base syntax/parse racket/syntax
                     syntax/stx racket/sequence))
(provide (all-defined-out))

(begin-for-syntax
  (define-syntax-class p+n
    (pattern a:id
             #:attr pat #'a
             #:attr name #'a)
    (pattern [a:id b:id]
             #:attr pat #'b
             #:attr name #'a))


  (define-syntax-class AbCd
    (pattern (A:id b:id C:id d:id)
             #:attr p (format-id #'A "~a~a-~a~a" #'A #'b #'C #'d)
             #:attr pat #'(list (A (b)) (C (d)))
             )))

(define-syntax-rule (define-me [name pat] ...)
  (begin
    (define-match-expander name
      (syntax-parser
        [(_) #'pat]))
    ...))

(define-match-expander G
  (syntax-parser
    [(_ size)
     #'(?GPR #:size size)]))

(define-match-expander M
  (syntax-parser
    [(_ size)
     #'(Mref size _ _ _)]))

(define-match-expander E
  (syntax-parser
    [(_ size)
     #'(or (G size) (Mref size _ _ (or #f (Imm 8) (Imm 32))))]))

(define-match-expander I
  (syntax-parser
    [(_ size)
     #'(or (Immediate size _)
           (Relocate size _ #f))]))

(define-match-expander J
  (syntax-parser
    [(_ size)
     #'(or (Relocate size _ #t) (Immediate size _))]))

(define-match-expander O
  (syntax-parser
    [(_ size)
     #'(Mref size #f #f (Imm 64))]))

(define-match-expander V
  (syntax-parser
    [(_)
     #'(?XMM)]
    [(_ size)
     #'(?XMM #:size size)]))

(define-match-expander W
  (syntax-parser
    [(_ size)
     #'(or (V)
           (Mref size _ _ (or #f (Imm 8) (Imm 32))))]))

(define-me
  [b 8]
  [w 16]
  [d 32]
  [q 64]
  [o 128]
  
  [v (or 16 32 64)]
  [z (or 16 32)]
  [y (or 32 64)]
  [x (or 16 64)])

(define-syntax (define-pred-1-helper stx)
  (define-syntax-class id1
    (pattern (A:id b:id)
             #:attr p (format-id #'A "~a~a" #'A #'b)
             #:attr first #'A
             #:attr u #'b
             ))
  (syntax-parse stx
    [(_ group:id1 ...)
     #'(begin
         (~@
          (define-match-expander group.p
            (syntax-parser
              [(_) #'(list (group.first (group.u)))]
              [(_ x) #'(list (group.first (group.u)) x)])))
         ...)]))

(define-syntax (define-pred-1 stx)
  (syntax-parse stx
    [(_ (A:id ...) (b:id ...))
     #:with ([AA bb] ...)
     (for*/list ([A (in-syntax #'(A ...))]
                 [b (in-syntax #'(b ...))])
       (list A b))
     #'(define-pred-1-helper
         [AA bb] ...)]))

(define-pred-1
  [G E I O J M]
  [b w d q o v z y x])

(define-syntax (define-pred-2-helper stx)
  (syntax-parse stx
    [(_ g:AbCd ...)
     #'(define-me
         [g.p g.pat]
         ...)]))

(define-syntax (define-pred-2 stx)
  (syntax-parse stx
    [(_ ([A:id B:id] ...) ([c:id d:id] ...))
     #:with ([(AA BB) (cc dd)] ...)
     (for*/list ([AB (in-syntax #'([A B] ...))]
                 [cd (in-syntax #'([c d] ...))])
       (list AB cd))
     #'(define-pred-2-helper
         [AA cc BB dd] ...)]))

(define-pred-2
  ([G E]
   [E G]
   [G I]
   [I G]
   [E I]
   [I I]
   [G M]
   [E M])
  ([b b]
   [v b]
   [y b]))

(define-syntax (define-pred-2+ stx)
  (syntax-parse stx
    [(_ id:AbCd ...)
     #'(define-me
         [id.p id.pat] ...)]))

(define-pred-2+
  (G y E w)
  (G d E w)
  (I w I b)
  (G q E d))

(define-match-expander V-V
  (syntax-parser
    [(_)
     #'(list (V) (V))]))

(define-match-expander VW
  (syntax-parser
    [(_ size)
     #'(list (V) (W size))]))

(define-match-expander WV
  (syntax-parser
    [(_ size)
     #'(list (W size) (V))]))

(define-match-expander VM
  (syntax-parser
    [(_ size)
     #'(list (V) (M size))]))

(define-match-expander MV
  (syntax-parser
    [(_ size)
     #'(list (M size) (V))]))

(define-match-expander vv
  (syntax-parser
    [(_ A B)
     #'(list (A (and (or 16 32 64) size)) (B size))]))

(define-match-expander vz
  (syntax-parser
    [(_ A B)
     #'(or (list (A 16) (B 16))
           (list (A 32) (B 32))
           (list (A 64) (B 32)))]))

(define-match-expander yy
  (syntax-parser
    [(_ A B)
     #'(list (A (and (or 32 64) size)) (B size))]))

(define-match-expander vvb
  (syntax-parser
    [(_ A B C)
     #'(list (A (and (or 16 32 64) size)) (B size) (C 8))]))

(define-match-expander vvz
  (syntax-parser
    [(_ A B C)
     #'(or (list (A 16) (B 16) (C 16))
           (list (A 32) (B 32) (C 32))
           (list (A 64) (B 64) (C 32)))]))

(define-syntax (define-pred-2* stx)
  (define-syntax-class id2
    (pattern (A:p+n b:id C:p+n d:id)
             #:attr p (format-id #'A "~a~a-~a~a"
                                 #'A.name #'b
                                 #'C.name #'d)
             #:attr first #'A.pat
             #:attr second #'C.pat
             #:attr u (format-id #'A "~a~a" #'b #'d)
             ))
  (syntax-parse stx
    [(_ group:id2 ...)
     #'(begin
         (~@
          (define-match-expander group.p
            (syntax-parser
              [(_) #'(group.u group.first group.second)])))
         ...)]))

(define-pred-2*
  (G v E v)
  (G v E z)
  (G v M v)
  (E v G v)
  (E v I v)
  (E v I z)
  (G v I v)
  (G v I z)
  (G v O v)
  (O v G v)
  (G y E y))

(define-match-expander bb
  (syntax-parser
    [(_ A B)
     #'(list (A 8) (B 8))]))

(define-me
  [Eb-CL (list (E (b)) (CL))]
  [Ev-CL (list (E (v)) (CL))]
  [AL-Ib (bb AL I)]
  [Ib-AL (bb I AL)]
  [AL-Ob (bb AL O)]
  [AL-Gb (bb AL G)]
  [AL-DX (list (AL) (DX))]
  [DX-AL (list (DX) (AL))]
  [rAX-Iz (vz ?rAX I)]
  [rAX-Oz (vz ?rAX O)]
  [rAX-Ov (vv ?rAX O)]
  [rAX-Gv (vv ?rAX G)]
  [eAX-Ib (list (I 8) (?rAX (z)))]
  [eAX-DX (list (?rAX (z)) (DX))]
  [Ib-eAX (list (I 8) (?rAX (z)))]
  [DX-eAX (list (DX) (?rAX (z)))]
  [Gv-rAX (vv G ?rAX)]
  [Gv-Ev-Ib (vvb G E I)]
  [Gv-Ev-Iz (vvz G E I)]
  [Ev-Gv-Ib (vvb E G I)]
  [Ev-Gv-CL (vvb E G CL)]
  [V-Wo (VW 128)]
  [V-Wq (VW 64)]
  [V-Wd (VW 32)]
  [V-Mo (VM 128)]
  [V-Mq (VM 64)]
  [V-Md (VM 32)]
  [V-Wo-Ib (list (V) (W 128) (I 8))]
  [V-Wq-Ib (list (V) (W 64) (I 8))]
  [V-Wd-Ib (list (V) (W 32) (I 8))]
  [Wo-V (WV 128)]
  [Wq-V (WV 64)]
  [Wd-V (WV 32)]
  [Mo-V (MV 128)]
  [V-Ib (list (V) (I 8))]
  [Gd-V (list (G 32) (V))]
  [Gd-V-Ib (list (G 32) (V) (I 8))]
  [Gy-V (list (G (y)) (V))]
  [Gy-Wq (list (G (y)) (W 64))]
  [Gy-Wd (list (G (y)) (W 32))]
  [V-Eq (list (V) (E 64))]
  [V-Ed (list (V) (E 32))]
  [Eq-V (list (E 64) (V))]
  [Ed-V (list (E 32) (V))]
  [V-GdMw-Ib (list (V) (or (G 32) (M 16)) (I 8))]
  [Eb-1 (Eb (Immediate _ 1))]
  [Ev-1 (Ev (Immediate _ 1))])