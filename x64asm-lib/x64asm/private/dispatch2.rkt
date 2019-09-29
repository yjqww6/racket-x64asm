#lang typed/racket/base
(require "encode.rkt" "assembler.rkt" "registers.rkt" "operand.rkt"
         "cases3.rkt" "patterns.rkt" "trace.rkt"
         racket/match
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
  
  (define ((make-case c caller) gs)
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
                     [c c]
                     [caller caller])
         (with-syntax ([(g ...) (make-guard guard (syntax->list #'(arg ...)))])
           #'[(c arg ...)
              (with-continuation-mark trace-key caller
                (pred-tree
                 (arg ...) (ss ...) (report-invalid-operands (list arg ...) caller)
                 [p (and g (s ss ...)) (e c arg ...)]
                 ...))]))]))

  (define dispatchers '())
  
  (define (same-dispatcher? a b)
    (let f ([p #`(#,a . #,b)])
      (syntax-case p ()
        [(a . b)
         (and (identifier? #'a)
              (identifier? #'b))
         (free-identifier=? #'a #'b)]
        [((a . b) . (c . d))
         (and (f #'(a . c)) (f #'(b . d)))]
        [(a . b)
         (eq? (syntax-e #'a) (syntax-e #'b))])))

  (define (make-dispatcher-helper pats caller)
    (define-syntax-class T
      (pattern (a)
               #:attr T #'a)
      (pattern (b ...)
               #:attr T #'(U b ...)))
    (with-syntax ([(e ...) (generate-temporaries pats)]
                  [caller caller])
      (define gs
        (for/list ([p (in-list pats)]
                   [e (in-list (syntax->list #'(e ...)))])
          (syntax-parse p #:literals (quote)
            ['() (vector 0 '() #f #f e '())]
            [(p:id)
             (match (hash-ref pred-table (syntax-e #'p))
               [(vector n pat guard size T)
                (vector n pat guard size e T)])])))
      (define c (generate-temporary 'c))
      (define cases (map (make-case c #'caller)
                         (group-by (λ (x) (vector-ref x 0)) gs)))
      (syntax-parse gs
        [(#(_ _ _ _ _ (T:T ...)) ...)
         (define t 
           #'(case->
              (-> Context T.T ... Void)
              ...))
         (values
          (let ()
            (cond
              [(assf (λ (x) (same-dispatcher? x pats)) dispatchers)
               => cdr]
              [else
               (define id
                 (with-syntax ([(clauses ...) cases])
                   (syntax-local-lift-expression
                    #`(λ ([caller : Symbol] [e : (-> Context T.T ... Void)] ...)
                        (ann
                         (case-lambda
                           clauses ...
                           [(#,c . r) (report-invalid-operands r caller)])
                         (-> Context Operand * Void))))))
               (set! dispatchers (cons (cons pats id) dispatchers))
               id]))
          t)])))

  
  (define (make-dispatcher pats eids caller)
    (let-values ([(id t) (make-dispatcher-helper pats caller)])
      (values #`(#,id '#,caller #,@eids) t))
    ))

(begin-for-syntax
  (define-syntax-class (pred prefix)
    (pattern '()
             #:attr unsafe (format-id prefix "~a:" prefix))
    (pattern (f:id)
             #:attr unsafe (format-id prefix "~a:~a" prefix (syntax-e #'f)))))

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
     #:with proc (format-id #'orig "proc:~a" #'name)
     #:do [(define-values (dispatcher types)
             (make-dispatcher
              (syntax->list #'(pat ...))
              (syntax->list #'(e ...))
              #'name))]
     #:with T types
     #`(begin
         (: orig (-> Context Operand * Void))
         (define-values (orig pat.unsafe ...)
           (let ([e enc] ...)
             (values #,dispatcher e ...)))
         (: proc (-> [#:ctx Context] Operand * Void))
         (define proc
           (λ (#:ctx [c (assert (current-context))]
               . r)
             (apply orig c r)))
         (module+ base
           (provide (rename-out [orig name])))
         (module+ procedure
           (provide proc))
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

#;
(define-dispatch here
  ['() void]
  [(Gv-Ev) void]
  [(V-Wo) void]
  [(Gv-Ev-Ib) void])
