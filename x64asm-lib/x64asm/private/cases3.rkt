#lang typed/racket/base
(require "patterns.rkt" "operand.rkt" "registers.rkt" "sizes.rkt"
         racket/match
         (for-syntax racket/base racket/match
                     syntax/parse racket/syntax
                     syntax/stx racket/sequence))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-syntax define-reg-pred
  (syntax-parser
    [(_ [name reg T] ...)
     #'(begin
         (~@
          (: name (-> T Boolean))
          (define (name a)
            (eq? a reg)))
         ...)]))

(define-reg-pred
  [AL? al GPR]
  [CL? cl GPR]
  [DX? dx GPR]
  [FS? fs Seg]
  [GS? gs Seg])

(: eAX? (-> GPR Boolean))
(define (eAX? x)
  (or (eq? x ax)
      (eq? x eax)))

(: rAX? (-> GPR Boolean))
(define (rAX? x)
  (or (eq? x ax)
      (eq? x eax)
      (eq? x rax)))

(: one? (-> Imm Boolean))
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
    [(_ name:id n:number (p:id ...) #f size-pred:id (T ...))
     #'(begin-for-syntax
         (hash-set! pred-table 'name
                    (vector n '(p ...) #f #'size-pred (list #'T ...))))]
    [(_ name:id n:number (p:id ...) (guard ...) size-pred:id (T ...))
     #'(begin-for-syntax
         (hash-set! pred-table 'name
                    (vector n '(p ...) (list #'guard ...) #'size-pred (list #'T ...))))]))

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
       #'(begin (define-pred id.p 1 (id.first) #f id.u ((id.T ...)))
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
          (define-pred id.p 2 (id.A id.C) #f id.u
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
          (define-pred id.p 2 (id.A id.C) #f id.u
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
      [(_ [name (A:id1 ...) u #:guard g ...] r ...)
       #:with n (datum->syntax #'k
                               (length (syntax->list #'(A ...))))
       #'(begin (define-pred name n (A ...) (g ...) u ((A.T ...) ...))
                (define-pred+ r ...))]
      [(_ [name (A:id1 ...) u] r ...)
       #:with n (datum->syntax #'k
                               (length (syntax->list #'(A ...))))
       #'(begin (define-pred name n (A ...) #f u ((A.T ...) ...))
                (define-pred+ r ...))])))

(define-syntax-rule (yes _ ...) #t)

(define-syntax (define-W stx)
  (define-syntax-class xx
    (pattern x:id
             #:attr name (format-id #'x "W~a?" #'x)))
  (syntax-parse stx
    [(_ a:xx ...)
     #'(begin
         (define (a.name [x : (U XMM Mref)])
           (if (Mref? x)
               (a (Mref-size x))
               #t))
         ...)]))

(define-W o q d w)
(define (Ib? [x : Imm])
  (b (Imm-size x)))

(define (Gy? [x : GPR])
  (y (Reg-size x)))

(define-pred+
  [V-V (V V) oo]
  [Eb-CL (E G) bb #:guard yes CL?]
  [Ev-CL (E G) vb #:guard yes CL?]
  [AL-Ib (G I) bb #:guard AL? yes]
  [Ib-AL (I G) bb #:guard yes AL?]
  [AL-Ob (G O) bb #:guard AL? yes]
  [AL-Gb (G G) bb #:guard AL? yes]
  [AL-DX (G G) bw #:guard AL? DX?]
  [DX-AL (G G) wb #:guard DX? AL?]
  [rAX-Iz (G I) vz #:guard rAX? yes]
  [rAX-Oz (G O) vz #:guard rAX? yes]
  [rAX-Ov (G O) vv #:guard rAX? yes]
  [rAX-Gv (G G) vv #:guard rAX? yes]
  [eAX-Ib (G I) zb #:guard eAX? yes]
  [eAX-DX (G G) zw #:guard eAX? DX?]
  [Ob-AL (O G) bb #:guard yes AL?]
  [Ov-rAX (O G) vv #:guard yes rAX?]
  [Ib-eAX (I G) bz #:guard yes eAX?]
  [DX-eAX (G G) wz #:guard DX? eAX?]
  [Gv-rAX (G G) vv #:guard yes rAX?]
  [Gv-Ev-Ib (G E I) vvb]
  [Gv-Ev-Iz (G E I) vvz]
  [Ev-Gv-Ib (E G I) vvb]
  [Ev-Gv-CL (E G G) vvb #:guard yes yes CL?]
  [V-Wo (V W) yes #:guard yes Wo?]
  [V-Wq (V W) yes #:guard yes Wq?]
  [V-Wd (V W) yes #:guard yes Wd?]
  [V-Mo (V M) oo]
  [V-Mq (V M) oq]
  [V-Md (V M) od]
  [V-Wo-Ib (V W I) yes #:guard yes Wo? Ib?]
  [V-Wq-Ib (V W I) yes #:guard yes Wq? Ib?]
  [V-Wd-Ib (V W I) yes #:guard yes Wd? Ib?]
  [Wo-V (W V) yes #:guard Wo? yes]
  [Wq-V (W V) yes #:guard Wq? yes]
  [Wd-V (W V) yes #:guard Wd? yes]
  [Mo-V (M V) oo]
  [V-Ib (V I) ob]
  [Gd-V (G V) do]
  [Gd-V-Ib (G V I) dob]
  [Gy-V (G V) yo]
  [Gy-Wq (G W) yes #:guard Gy? Wq?]
  [Gy-Wd (G W) yes #:guard Gy? Wd?]
  [V-Eq (V E) oq]
  [V-Ed (V E) od]
  [Eq-V (E V) qo]
  [Ed-V (E V) do]
  [V-Gd-Ib (V G I) odb]
  [V-Mw-Ib (V M I) owb]
  [Eb-1 (E I) bb #:guard yes one?]
  [Ev-1 (E I) vb #:guard yes one?]
  [Gv-S (G S) vw]
  [Mw-S (M S) ww]
  [S-Ew (S E) ww]
  [FS (S) w #:guard FS?]
  [GS (S) w #:guard GS?])