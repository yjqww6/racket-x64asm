#lang typed/racket/base

(require racket/match
         (for-syntax racket/base syntax/parse racket/syntax))
(provide (all-defined-out))

(define-syntax define-rule
  (syntax-parser
    [(_ (f:id arg:id ...+) body:expr ...+)
     #:with (tmp ...) (generate-temporaries #'(arg ...))
     #'(define-syntax-rule (f tmp ...)
         (let ([arg tmp] ...)
           body ...))]))

(define-syntax define-size
  (syntax-parser
    [(_ [name:id size:number ...] ...)
     #'(begin
         (define-rule (name s)
           (or (eq? s size) ...))
         ...)]))

(define-size
  [b 8]
  [w 16]
  [d 32]
  [q 64]
  [o 128]
  
  [v 16 32 64]
  [z 16 32]
  [y 32 64]
  [x 16 64])

(define-rule (vv a b)
  (and (v a)
       (eq? a b)))

(define-rule (yy a b)
  (and (y a)
       (eq? a b)))

(define-rule (vz a b)
  (match* (a b)
    [(16 16) #t]
    [(32 32) #t]
    [(64 32) #t]
    [(_ _) #f]))

(define-rule (vvz a b c)
  (match* (a b c)
    [(16 16 16) #t]
    [(32 32 32) #t]
    [(64 64 32) #t]
    [(_ _ _) #f]))

(begin-for-syntax
  (define-syntax-class u
    (pattern (u v)
             #:attr name (format-id #'u "~a~a" #'u #'v))))

(define-syntax define-size-pred
  (syntax-parser
    [(_ name:id u:u)
     #'(define-rule (name a b)
         (and (u.u a)
              (u.v b)))]))

(define-syntax define-size-pred+
  (syntax-parser
    [(_ u:u ...)
     #'(begin
         (define-size-pred u.name [u.u u.v])
         ...)]))

(define-syntax (define-size-pred* stx)
  (syntax-parse stx
    [(_ [a:id ...] [b:id ...])
     #:with ([u v] ...)
     (for*/list ([a (in-list (syntax->list #'(a ...)))]
                 [b (in-list (syntax->list #'(b ...)))])
       (list a b))
     #'(define-size-pred+ [u v] ...)]))

(define-size-pred*
  [b w d q o]
  [b w d q o])

(define-size-pred*
  [v z y]
  [b w d q o])

(define-size-pred+
  [b z]
  [w z])

(define-syntax define-size-pred3
  (syntax-parser
    [(_) #'(begin)]
    [(_ [a:id b:id c:id] r ...)
     #:with name (format-id #'a "~a~a~a" #'a #'b #'c)
     #:with ab (format-id #'a "~a~a" #'a #'b)
     #'(begin
         (define-rule (name x y z)
           (and (ab x y)
                (c z)))
         (define-size-pred3 r ...))]))

(define-size-pred3
  [v v b]
  [o o b]
  [o q b]
  [o d b]
  [o w b]
  [q o b]
  [d o b])
