#lang typed/racket/base
(require "encode.rkt" "assembler.rkt" "registers.rkt" "operand.rkt"
         "cases3.rkt" "patterns.rkt"
         racket/match
         #;(rename-in racket/base
                      [define-syntax base:defstx])
         (for-syntax racket/base racket/match
                     syntax/parse syntax/name racket/list
                     racket/syntax syntax/id-table))
(provide (all-defined-out))

(begin-for-syntax
  (define (make-guard guard args)
    (with-syntax ([(arg ...) args])
      (for/list ([g (in-list guard)])
        (cond
          [(not g) #'#t]
          [else
           (with-syntax ([(g ...) g])
             #`(and (g arg) ...))]))))
  
  (define ((make-case c) gs)
    (match gs
      [(list (vector 0 '() #f #f e '()))
       #`[(#,c) (#,e #,c)]]
      [(list (vector n pat guard size e _) ...)
       (with-syntax ([(arg ...) (generate-temporaries (build-list
                                                       (car n)
                                                       (λ (_) 'arg)))]
                     [(ss ...) (generate-temporaries (build-list
                                                      (car n)
                                                      (λ (_) 'ss)))]
                     [(p ...) pat]
                     [(s ...) size]
                     [(e ...) e]
                     [c c])
         (with-syntax ([(g ...) (make-guard guard (syntax->list #'(arg ...)))])
           #'[(c arg ...)
              (pred-tree
               (arg ...) (ss ...) (error 'encode "invalid: ~a" (list arg ...))
               [p (and g (s ss ...)) (e c arg ...)]
               ...)]))]))
  
  (define (make-dispatcher pats eids)
    (define-syntax-class T
      (pattern (a)
               #:attr T #'a)
      (pattern (b ...)
               #:attr T #'(U b ...)))
    (define gs
      (for/list ([p (in-list pats)]
                 [e (in-list eids)])
        (syntax-parse p #:literals (quote)
          ['() (vector 0 '() #f #f e '())]
          [(p:id)
           (match (hash-ref pred-table (syntax-e #'p))
             [(vector n pat guard size T)
              (vector n pat guard size e T)])])))
    (define c (generate-temporary 'c))
    (define cases (map (make-case c) (group-by (λ (x) (vector-ref x 0)) gs)))
    (values
     (with-syntax ([(clauses ...) cases])
       #`(case-lambda
           clauses ...
           [(#,c . r) (error 'encode "invalid: ~a" r)]))
     (syntax-parse gs
       [(#(_ _ _ _ _ (T:T ...)) ...)
        #'(case->
           (-> Context T.T ... Void)
           ...)]))
    ))

(begin-for-syntax
  (define-syntax-class (pred prefix)
    (pattern '()
             #:attr unsafe (format-id prefix "~a:" prefix))
    (pattern (f:id)
             #:attr unsafe (format-id prefix "~a:~a" prefix (syntax-e #'f)))))

(module proxy racket/base
  (require (for-syntax racket/base syntax/parse)
           (only-in typed/racket/base assert)
           "assembler.rkt")
  (provide define-proxy)
  
  (define-syntax define-proxy
    (syntax-parser
      [(_ name ls proc)
       #'(...
          (begin
            (define-syntax name
              (syntax-parser 
                [(_ (~alt (~optional (~seq #:ctx c)
                                     #:defaults ([c #'(assert (current-context))]))
                          args)
                    ...)
                 #'(ls c args ...)]
                [f:id
                 #'proc]))
            (provide name)))])))
(require 'proxy)

(define-syntax dispatcher
  (syntax-parser
    [(_ [pat e] ...)
     (define-values (a b)
       (make-dispatcher
        (syntax->list #'(pat ...))
        (syntax->list #'(e ...))))
     a]))

(define-syntax define-dispatch-0
  (syntax-parser
    [(_ name [pat enc] ...)
     #:declare pat (pred #'name)
     #:with (e ...) (generate-temporaries #'(enc ...))
     #:with orig (generate-temporary 'ls)
     #:with tmp (generate-temporary 'tmp)
     #:with proc (generate-temporary 'proc)
     #:do [(define-values (dispatcher types)
             (make-dispatcher
              (syntax->list #'(pat ...))
              (syntax->list #'(e ...))))]
     #:with T types
     #`(begin
         (: orig (-> Context Operand * Void))
         (define-values (orig pat.unsafe ...)
           (let ([e enc] ...)
             (values (ann #,dispatcher
                          (-> Context Operand * Void))
                     e ...)))
         (: proc (-> [#:ctx Context] Operand * Void))
         (define proc
           (λ (#:ctx [c (assert (current-context))]
               . r)
             (apply orig c r)))
         (define-proxy name orig proc)
         (module+ ls
           (define (name [c : Context] [l : (Listof Operand)])
             (apply orig c l))
           (provide name))
         (module+ well-typed
           (: tmp T)
           (define tmp orig)
           (provide (rename-out [tmp name])))
         (module+ unsafe
           (provide pat.unsafe ...)))]))

(define-syntax (define-dispatch stx)
  (define-syntax-class names
    (pattern (n:id ns:id ...)
             #:attr name #'n
             #:attr ls (format-id #'n "ls:~a" #'n)
             #:attr (alias 1) (syntax->list #'(ns ...)))
    (pattern n:id
             #:attr name #'n
             #:attr ls (format-id #'n "ls:~a" #'n)
             #:attr (alias 1) '()))
  (syntax-parse stx
    [(_ () p ...) #'(begin)]
    [(_ name:names [pat enc] ...)
     #'(begin
         (define-dispatch-0 name.name [pat enc] ...)
         (define-dispatch-0 name.alias [pat enc] ...)
         ...)]))


(define-dispatch here
  ['() void]
  [(Gv-Ev) void]
  [(V-Wo) void]
  [(Gv-Ev-Ib) void])
