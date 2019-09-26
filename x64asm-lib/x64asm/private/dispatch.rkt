#lang typed/racket/base
(require "encode.rkt" "assembler.rkt" "registers.rkt" "operand.rkt"
         "cases2.rkt"
         racket/match
         (for-syntax racket/base syntax/parse syntax/name
                     racket/syntax syntax/id-table))
(provide (all-defined-out))

(begin-for-syntax
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
  
  (define (make-dispatcher preds)
    (define slot (assf (λ (x) (same-dispatcher? x preds)) dispatchers))
    (cond
      [slot => cdr]
      [else (syntax-parse preds
              [(pred ...)
               #:with (args ...) (generate-temporaries #'(pred ...))
               (define id
                 (syntax-local-lift-expression
                  #'(λ ([args : Encoder] ...)
                      (: name (-> (Listof Operand) [#:ctx (Option Context)] Void))
                      (define (name #:ctx [ctx (current-context)] ops)
                        (assert ctx)
                        (match ops
                          [pred (args ctx ops)] ...))
                      name)))
               (set! dispatchers (cons (cons preds id) dispatchers))
               id])]))

  (define (reduce-type ls)
    (let loop ([ls ls])
      (cond
        [(null? ls) '()]
        [else
         (cons (car ls)
               (loop (filter (λ (b) (not (same-dispatcher? (car ls) b)))
                             (cdr ls))))])))
  
  (define-syntax-class (pred prefix)
    (pattern '()
             #:attr unsafe (format-id prefix "~a:" prefix)
             #:attr T #'())
    (pattern (f:id)
             #:attr unsafe (format-id prefix "~a:~a" prefix (syntax-e #'f))
             #:attr T (lookup-compound (syntax-local-introduce #'f))))
  (define-syntax-class names
    (pattern (n:id ns:id ...)
             #:attr name #'n
             #:attr ls (format-id #'n "ls:~a" #'n)
             #:attr (alias 1) (syntax->list #'(ns ...)))
    (pattern n:id
             #:attr name #'n
             #:attr ls (format-id #'n "ls:~a" #'n)
             #:attr (alias 1) '()))
  )

(define-syntax (define-dispatch stx)
  (syntax-parse stx
    [(_ () r ...) #'(begin)]
    [(_ name:names [p encoder] ...)
     #:declare p (pred #'name.name)
     #:with id (make-dispatcher (syntax-local-introduce #'(p ...)))
     #:with (e ...) (generate-temporaries #'(encoder ...))
     #:with (T ...) (reduce-type (syntax->list #'(p.T ...)))
     #:with ((C ...) ...) #'(T ...)
     #:with tmp (generate-temporary 'tmp)
     #'(begin
         (define-values (name.ls name.name p.unsafe ...)
           (let ([e encoder] ...)
             (define name.ls (id e ...))
             (values
              name.ls
              (ann (λ (#:ctx [ctx (current-context)] . rst)
                     (name.ls #:ctx ctx rst))
                   (-> [#:ctx (Option Context)] Operand * Void))
              e ...)))
         (define name.alias name.name) ...
         (provide name.name name.alias ...)
         (module+ ls
           (provide (rename-out [name.ls name.name])))
         (module+ unsafe
           (provide p.unsafe ...))
         (module+ well-typed
           (: tmp
              (case->
               (-> C ... Void)
               ...))
           (define tmp name.name)
           (provide (rename-out [tmp name.name]))))]))