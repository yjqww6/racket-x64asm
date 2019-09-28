#lang typed/racket/base

(require racket/match "registers.rkt"
         (for-syntax racket/base syntax/parse racket/syntax))
(provide (all-defined-out))

(define-syntax define-size
  (syntax-parser
    [(_ [name:id size:number ...] ...)
     #'(begin
         (define (name [s : Size])
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

(define-type Size-Pred-2 (-> Size Size Boolean))
(define-type Size-Pred-3 (-> Size Size Size Boolean))

(: vv Size-Pred-2)
(define (vv a b)
  (and (v a)
       (eq? a b)))

(: yy Size-Pred-2)
(define (yy a b)
  (and (y a)
       (eq? a b)))

(: vz Size-Pred-2)
(define (vz a b)
  (match* (a b)
    [(16 16) #t]
    [(32 32) #t]
    [(64 32) #t]
    [(_ _) #f]))

(: vvz Size-Pred-3)
(define (vvz a b c)
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
     #'(define (name [a : Size] [b : Size])
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
         (define (name [x : Size] [y : Size] [z : Size])
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
