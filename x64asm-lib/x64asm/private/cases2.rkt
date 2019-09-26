#lang typed/racket/base

(require "operand.rkt" "registers.rkt"
         racket/match
         (for-syntax racket/base syntax/parse racket/syntax
                     syntax/stx racket/sequence))
(provide (all-defined-out) (for-syntax lookup-compound))

(begin-for-syntax
  (define-syntax-class AbCd
    (pattern (A:id b:id C:id d:id)
             #:attr p (format-id #'A "~a~a-~a~a" #'A #'b #'C #'d)
             #:attr pat #'(list (A (b)) (C (d)))
             #:attr (T 1)
             (let ([ta (lookup-atom #'A)]
                   [tb (lookup-atom #'C)])
               (list ta tb))
             )))

(define-syntax define-me 
  (syntax-parser
    [(_ [name pat (~optional (~seq (~literal :) T ...))] ...) 
     #'(begin
         (~@
          (define-match-expander name
            (syntax-parser
              [(_) #'pat]))
          (~? (declare! name T ...)))
         ...)]))

(define-for-syntax atom-types (make-hasheq))
(define-for-syntax compound-types (make-hasheq))

(define-for-syntax (lookup-atom id [f (λ () (error 'lookup-atom "~a" id))])
  (hash-ref atom-types (syntax-e id) f))
(define-for-syntax (lookup-compound id)
  (hash-ref compound-types (syntax-e id)))

(define-syntax attach!
  (syntax-parser
    [(_ id:id T)
     #'(begin-for-syntax
         (hash-set! atom-types 'id #'T))]))
(define-syntax declare!
  (let ()
    (define-syntax-class T
      (pattern id:id
               #:attr T (lookup-atom #'id (λ () #'id)))
      (pattern T))
    (syntax-parser
      [(_ id:id T:T ...)
       #'(begin-for-syntax
           (hash-set! compound-types 'id #'(T.T ...)))])))

(define-match-expander G
  (syntax-parser
    [(_ size)
     #'(?GPR #:size size)]))
(attach! G GPR)

(define-match-expander M
  (syntax-parser
    [(_ size)
     #'(Mref size _ _ (or #f (Imm 8 _) (Imm 32 _)) _)]))
(attach! M Mref)

(define-match-expander E
  (syntax-parser
    [(_ size)
     #'(or (G size) (M size))]))
(attach! E (U GPR Mref))

(define-match-expander I
  (syntax-parser
    [(_ size)
     #'(Imm size _)]))
(attach! I Imm)

(define-match-expander J
  (syntax-parser
    [(_ size)
     #'(Imm size _)]))
(attach! J Imm)

(define-match-expander O
  (syntax-parser
    [(_ size)
     #'(Offset size (Imm 64 _) _)]))
(attach! O Offset)

(define-match-expander V
  (syntax-parser
    [(_)
     #'(?XMM)]
    [(_ size)
     #'(?XMM #:size size)]))
(attach! V XMM)

(define-match-expander W
  (syntax-parser
    [(_ size)
     #'(or (V)
           (M size))]))
(attach! W (U XMM Mref))

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
             #:attr T (lookup-atom #'A)
             ))
  (syntax-parse stx
    [(_ group:id1 ...)
     #'(begin
         (~@
          (define-match-expander group.p
            (syntax-parser
              [(_) #'(list (group.first (group.u)))]
              [(_ x) #'(list (group.first (group.u)) x)]))
          (declare! group.p group.T))
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
         [g.p g.pat : g.T ...]
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
         [id.p id.pat : id.T ...] ...)]))

(define-pred-2+
  (G y E w)
  (G d E w)
  (I w I b)
  (G q E d))

(define-match-expander V-V
  (syntax-parser
    [(_)
     #'(list (V) (V))]))
(declare! V-V XMM XMM)

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
    (pattern (A:id b:id C:id d:id)
             #:attr p (format-id #'A "~a~a-~a~a"
                                 #'A #'b
                                 #'C #'d)
             #:attr first #'A
             #:attr second #'C
             #:attr u (format-id #'A "~a~a" #'b #'d)
             #:attr (T 1)
             (let ([ta (lookup-atom #'A)]
                   [tb (lookup-atom #'C)])
               (list ta tb))
             ))
  (syntax-parse stx
    [(_ group:id2 ...)
     #'(begin
         (~@
          (define-match-expander group.p
            (syntax-parser
              [(_) #'(group.u group.first group.second)]))
          (declare! group.p group.T ...))
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
  [Eb-CL (list (E (b)) (CL)) : E G]
  [Ev-CL (list (E (v)) (CL)) : E G]
  [AL-Ib (bb AL I) : G I]
  [Ib-AL (bb I AL) : I G]
  [AL-Ob (bb AL O) : G O]
  [AL-Gb (bb AL G) : G G]
  [AL-DX (list (AL) (DX)) : G G]
  [DX-AL (list (DX) (AL)) : G G]
  [rAX-Iz (vz ?rAX I) : G I]
  [rAX-Oz (vz ?rAX O) : G O]
  [rAX-Ov (vv ?rAX O) : G O]
  [rAX-Gv (vv ?rAX G) : G G]
  [eAX-Ib (list (I 8) (?rAX (z))) : G I]
  [eAX-DX (list (?rAX (z)) (DX)) : G G]
  [Ib-eAX (list (I 8) (?rAX (z))) : I G]
  [DX-eAX (list (DX) (?rAX (z))) : G G]
  [Gv-rAX (vv G ?rAX) : G G]
  [Gv-Ev-Ib (vvb G E I) : G E I]
  [Gv-Ev-Iz (vvz G E I) : G E I]
  [Ev-Gv-Ib (vvb E G I) : E G O]
  [Ev-Gv-CL (vvb E G CL) : E G G]
  [V-Wo (VW 128) : V W]
  [V-Wq (VW 64) : V W]
  [V-Wd (VW 32) : V W]
  [V-Mo (VM 128) : V M]
  [V-Mq (VM 64) : V M]
  [V-Md (VM 32) : V M]
  [V-Wo-Ib (list (V) (W 128) (I 8)) : V W I]
  [V-Wq-Ib (list (V) (W 64) (I 8)) : V W I]
  [V-Wd-Ib (list (V) (W 32) (I 8)) : V W I]
  [Wo-V (WV 128) : W V]
  [Wq-V (WV 64) : W V]
  [Wd-V (WV 32) : W V]
  [Mo-V (MV 128) : W V]
  [V-Ib (list (V) (I 8)) : V I]
  [Gd-V (list (G 32) (V)) : G V]
  [Gd-V-Ib (list (G 32) (V) (I 8)) : G V I]
  [Gy-V (list (G (y)) (V)) : G V]
  [Gy-Wq (list (G (y)) (W 64)) : G W]
  [Gy-Wd (list (G (y)) (W 32)) : G W]
  [V-Eq (list (V) (E 64)) : V E]
  [V-Ed (list (V) (E 32)) : V W]
  [Eq-V (list (E 64) (V)) : E V]
  [Ed-V (list (E 32) (V)) : E V]
  [V-GdMw-Ib (list (V) (or (G 32) (M 16)) (I 8)) : V E I]
  [Eb-1 (Eb (Immediate _ #f 1)) : E I]
  [Ev-1 (Ev (Immediate _ #f 1)) : E I]
  [Gv-S (list (G (v)) (? Seg?)) : G Seg]
  [Mw-S (list (Mref 16 _ _ _ _) (? Seg?)) : M Seg]
  [S-Ew (list (? Seg?) (or (G 16) (Mref 16 _ _ _ #f))) : Seg E]
  [FS- (list (FS)) : Seg]
  [GS- (list (GS)) : Seg])