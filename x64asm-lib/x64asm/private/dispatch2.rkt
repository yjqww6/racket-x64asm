#lang typed/racket/base
(require "assembler.rkt" "operand.rkt"
         (for-syntax racket/base syntax/parse racket/syntax syntax/stx))
(provide define-dispatch)

(module dispatcher racket/base
  (require racket/match syntax/parse racket/list racket/syntax
           (for-template typed/racket/base "cases3.rkt" "trace.rkt" "patterns.rkt"
                         "assembler.rkt" "operand.rkt"))
  
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

  (define dispatchers (make-hash))

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
      (define key (map syntax->datum pats))
      (syntax-parse gs
        [(#(_ _ _ _ _ (T:T ...)) ...)
         (define t 
           #'(case->
              (-> Context T.T ... Void)
              ...))
         (values
          (let ()
            (cond
              [(hash-ref dispatchers key (λ () #f))
               => values]
              [else
               (define c (generate-temporary 'c))
               (define cases (map (make-case c #'caller)
                                  (group-by (λ (x) (vector-ref x 0)) gs)))
               (define id
                 (with-syntax ([(clauses ...) cases])
                   (syntax-local-lift-expression
                    #`(λ ([caller : Symbol] [e : (-> Context T.T ... Void)] ...)
                        (ann
                         (case-lambda
                           clauses ...
                           [(#,c . r) (report-invalid-operands r caller)])
                         (-> Context Operand * Void))))))
               (hash-set! dispatchers key id)
               id]))
          t)])))

  
  (define (make-dispatcher pats eids caller)
    (let-values ([(id t) (make-dispatcher-helper pats caller)])
      (values #`(#,id '#,caller #,@eids) t))
    )

  (define-syntax-class pred
    (pattern '()
             #:attr unsafe (λ (prefix) (format-id prefix "~a:" prefix)))
    (pattern (f:id)
             #:attr unsafe (λ (prefix) (format-id prefix "~a:~a" prefix (syntax-e #'f)))))
  
  (provide pred make-dispatcher))

(require (for-syntax 'dispatcher))

(define (make-proc [orig : (-> Context Operand * Void)]) : (-> [#:ctx Context] Operand * Void)
  (λ (#:ctx [c (assert (current-context))] . r)
    (apply orig c r)))

(define-syntax define-dispatch-0
  (syntax-parser
    [(_ name (~optional (~seq #:alias [alias:id ...]) #:defaults ([(alias 1) #'()]))
        [pat enc] ...)
     #:declare pat pred
     #:with ((pat-unsafe ...) (pat-unsafe-alias ...) ...)
     (stx-map (λ (a)
                (for/list ([f (in-list (attribute pat.unsafe))])
                  (f a)))
              #'(name alias ...))
     #:with (e ...) (generate-temporaries #'(enc ...))
     #:with orig (generate-temporary 'ls)
     #:with tmp (generate-temporary 'tmp)
     #:with (proc proc-alias ...)
     (stx-map (λ (a) (format-id #'orig "proc:~a" a)) #'(name alias ...))
     #:do [(define-values (dispatcher types)
             (make-dispatcher
              (syntax->list #'(pat ...))
              (syntax->list #'(e ...))
              #'name))]
     #:with T types
     #`(begin
         (: orig (-> Context Operand * Void))
         (define-values (orig pat-unsafe ...)
           (let ([e enc] ...)
             (values #,dispatcher e ...)))
         (provide (rename-out [orig name] [orig alias] ...))
         
         (module+ procedure
           (: proc (-> [#:ctx Context] Operand * Void))
           (define proc (procedure-rename (make-proc orig) 'name))
           (provide proc (rename-out [proc proc-alias] ...)))
         (module+ well-typed
           (: tmp T)
           (define tmp orig)
           (provide (rename-out [tmp name] [tmp alias] ...)))
         (module+ unsafe
           (provide
            pat-unsafe ...
            (rename-out
             [pat-unsafe pat-unsafe-alias]
             ...)
            ...)))]))

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
     #'(define-dispatch-0 name.name #:alias [name.alias ...] [pat enc] ...)]))

#;
(define-dispatch here
  ['() void]
  [(Gv-Ev) void]
  [(V-Wo) void]
  [(Gv-Ev-Ib) void])
