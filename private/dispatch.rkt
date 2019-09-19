#lang typed/racket/base
(require "encode.rkt" "assembler.rkt" "registers.rkt" "operand.rkt"
         racket/match
         (for-syntax racket/base syntax/parse syntax/name
                     racket/syntax))
(provide (all-defined-out))
#;
(define-syntax define-dispatch
  (syntax-rules ()
    [(_ () [pred encoder] ...)
     (begin)]
    [(_ (name names ...) [pred encoder] ...)
     (begin
       (define-dispatch name [pred encoder] ...)
       (~@ (define names name)
           (provide names))
       ...)]
    [(_ name [pred encoder] ...)
     (begin
       (: name ([#:ctx (Option Context)] Operand * -> Void))
       (define (name #:ctx [ctx (current-context)]
                     . ops)
         (assert ctx)
         (match ops
           [pred (encoder ctx ops)] ...))
       (provide name))]))

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
                      (: name ([#:ctx (Option Context)] Operand * -> Void))
                      (define (name #:ctx [ctx (current-context)]
                                    . ops)
                        (assert ctx)
                        (match ops
                          [pred (args ctx ops)] ...))
                      name)))
               (set! dispatchers (cons (cons preds id) dispatchers))
               id])]))
  (define-syntax-class (pred prefix)
    (pattern '()
             #:attr name (format-id prefix "unsafe-~a:" prefix))
    (pattern (f:id)
             #:attr name (format-id prefix "unsafe-~a:~a" prefix (syntax-e #'f))))
  (define-syntax-class names
    (pattern (n:id ns:id ...)
             #:attr name #'n
             #:attr (alias 1) (syntax->list #'(ns ...)))
    (pattern n:id
             #:attr name #'n
             #:attr (alias 1) '()))
  )

(define-syntax (define-dispatch stx)
  (syntax-parse stx
    [(_ () r ...) #'(begin)]
    [(_ name:names [p encoder] ...)
     #:declare p (pred #'name.name)
     #:with id (make-dispatcher (syntax-local-introduce #'(p ...)))
     #:with (e ...) (generate-temporaries #'(encoder ...))
     #'(begin
         (define-values (name.name p.name ...)
           (let ([e encoder] ...)
             (values
              (id e ...)
              (ann (λ (#:ctx [ctx (current-context)] . rst)
                     (e (assert ctx) rst))
                   (-> [#:ctx (Option Context)] Operand * Void))
              ...)))
         (define name.alias name.name) ...
         (provide name.name p.name ... name.alias ...))]))