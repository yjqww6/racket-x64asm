#lang typed/racket/base
(require "patterns.rkt" "operand.rkt" "registers.rkt" "sizes.rkt"
         racket/match
         (for-syntax racket/base racket/match
                     syntax/parse racket/syntax
                     syntax/stx racket/sequence))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-syntax define-reg-pred
  (syntax-parser
    [(_ [X name reg T T?] ...)
     #'(begin
         (~@
          (: name (-> Any Boolean : #:+ T))
          (define (name a)
            (and (eq? a reg)
                 (T? a)))
          (define-atom-pred X name Reg-size T))
         ...)]))

(define-reg-pred
  [AL AL? al GPR GPR?]
  [CL CL? cl GPR GPR?]
  [DX DX? dx GPR GPR?]
  [FS FS? fs Seg Seg?]
  [GS GS? gs Seg Seg?])

(: eAX? (-> Any Boolean : #:+ GPR))
(define (eAX? x)
  (and
   (or (eq? x ax)
       (eq? x eax))
   (GPR? x)))

(: rAX? (-> Any Boolean : #:+ GPR))
(define (rAX? x)
  (and
   (or (eq? x ax)
       (eq? x eax)
       (eq? x rax))
   (GPR? x)))

(: one? (-> Any Boolean : #:+ Imm))
(define (one? x)
  (and (Immediate? x)
       (eq? (Immediate-num x) 1)))

(define-atom-pred One one? Imm-size Imm)
(define-atom-pred eAX eAX? Reg-size GPR)
(define-atom-pred rAX rAX? Reg-size GPR)

(define-atom-pred G GPR? Reg-size GPR)
(define-atom-pred M Mref? Mref-size Mref)
(define-atom-pred I Imm? Imm-size Imm)
(define-atom-pred O Offset? Offset-size Offset)
(define-atom-pred V XMM? Reg-size XMM)
(define-atom-pred S Seg? Reg-size Seg)

(define-compound-pred E G M)
(define-compound-pred W V M)
(define-compound-pred J I)

(define-for-syntax pred-table (make-hasheq))

(define-syntax define-pred
  (syntax-parser
    [(_ name:id n:number (p:id ...) size-pred:id (T ...))
     #'(begin-for-syntax
         (hash-set! pred-table 'name
                    (vector n '(p ...) #'size-pred (list #'T ...))))]))

(define-syntax define-pred-1-helper
  (let ()
    (define-syntax-class id1
      (pattern (A:id b:id)
               #:attr p (format-id #'A "~a~a" #'A #'b)
               #:attr first #'A
               #:attr u #'b
               #:attr (T 1) (get-types (syntax-e #'A))
               ))
    (syntax-parser
      [(_ id:id1 ...)
       #'(begin (define-pred id.p 1 (id.first) id.u ((id.T ...)))
                ...)])))

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

(begin-for-syntax
  (define-syntax-class AbCd
    (pattern (A:id b:id C:id d:id)
             #:attr p (format-id #'A "~a~a-~a~a" #'A #'b #'C #'d)
             #:attr u (format-id #'b "~a~a" #'b #'d)
             #:attr (TA 1) (get-types (syntax-e #'A))
             #:attr (TC 1) (get-types (syntax-e #'C))
             )))

(define-syntax define-pred-2-helper
  (syntax-parser
    [(_ id:AbCd ...)
     #'(begin
         (~@
          (define-pred id.p 2 (id.A id.C) id.u
            ((id.TA ...) (id.TC ...))))
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

(define-pred-2-helper
  (G y E w)
  (G d E w)
  (I w I b)
  (G q E d))

(define-syntax (define-pred-2* stx)
  (syntax-parse stx
    [(_ id:AbCd ...)
     #'(begin
         (~@
          (define-pred id.p 2 (id.A id.C) id.u
            ((id.TA ...) (id.TC ...))))
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

(define-syntax define-pred+
  (let ()
    (define-syntax-class id1
      (pattern A:id
               #:attr (T 1) (get-types (syntax-e #'A))))
    (syntax-parser
      [(_) #'(begin)]
      [(_ [name (A:id1 ...) u] r ...)
       #:with n (datum->syntax #'k
                               (length (syntax->list #'(A ...))))
       #'(begin (define-pred name n (A ...) u ((A.T ...) ...))
                (define-pred+ r ...))])))

(define-pred+
  [V-V (V V) oo]
  [Eb-CL (E CL) bb]
  [Ev-CL (E CL) vb]
  [AL-Ib (AL I) bb]
  [Ib-AL (I AL) bb]
  [AL-Ob (AL O) bb]
  [AL-Gb (AL G) bb]
  [AL-DX (AL DX) bb]
  [DX-AL (DX AL) bb]
  [rAX-Iz (rAX I) vz]
  [rAX-Oz (rAX O) vz]
  [rAX-Ov (rAX O) vv]
  [rAX-Gv (rAX G) vv]
  [eAX-Ib (eAX I) zb]
  [eAX-DX (eAX DX) zb]
  [Ob-AL (O AL) bb]
  [Ov-rAX (O G) vv]
  [Ib-eAX (I eAX) bz]
  [DX-eAX (DX eAX) bz]
  [Gv-rAX (G rAX) vv]
  [Gv-Ev-Ib (G E I) vvb]
  [Gv-Ev-Iz (G E I) vvz]
  [Ev-Gv-Ib (E G I) vvb]
  [Ev-Gv-CL (E G CL) vvb]
  [V-Wo (V W) oo]
  [V-Wq (V W) oq]
  [V-Wd (V W) od]
  [V-Mo (V M) oo]
  [V-Mq (V M) oq]
  [V-Md (V M) od]
  [V-Wo-Ib (V W I) oob]
  [V-Wq-Ib (V W I) oqb]
  [V-Wd-Ib (V W I) odb]
  [Wo-V (W V) oo]
  [Wq-V (W V) qo]
  [Wd-V (W V) do]
  [Mo-V (M V) oo]
  [V-Ib (V I) ob]
  [Gd-V (G V) do]
  [Gd-V-Ib (G V I) dob]
  [Gy-V (G V) yo]
  [Gy-Wq (G W) yq]
  [Gy-Wd (G W) yd]
  [V-Eq (V E) oq]
  [V-Ed (V E) od]
  [Eq-V (E V) qo]
  [Ed-V (E V) do]
  [V-Gd-Ib (V G I) odb]
  [V-Mw-Ib (V M I) owb]
  [Eb-1 (E One) bb]
  [Ev-1 (E One) vb]
  [Gv-S (G S) vw]
  [Mw-S (M S) ww]
  [S-Ew (S E) ww]
  [FS- (FS) w]
  [GS- (GS) w])