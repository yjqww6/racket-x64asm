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
       (: name (#:ctx Context Operand * -> Void))
       (define (name #:ctx ctx
                     . ops)
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
                      (: name (#:ctx Context Operand * -> Void))
                      (define (name #:ctx ctx
                                    . ops)
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

(require racket/stxparam)

(define-syntax-parameter current-ctx #f)

(define-syntax with-ctx
  (syntax-parser
    [(_ id:id body ...)
     #`(syntax-parameterize ([current-ctx #'#,(syntax-local-introduce #'id)])
         body ...)]
    [(_ expr body ...)
     #`(let ([id expr])
         (with-ctx id
           body ...))]))

(define-syntax define-ctx
  (syntax-parser
    [(_ name body)
     #:with id (generate-temporary #'name)
     #'(...
        (begin
          (define id body)
          (define-syntax name
            (syntax-parser
              [(_ (~alt (~once (~seq #:ctx ctx))
                        args)
                  ...)
               #'(id #:ctx ctx args ...)]
              [(_ args ...)
               #:do [(define par (syntax-parameter-value #'current-ctx))]
               #:when par
               #`(id #:ctx #,par args ...)]
              [a
               #:when (identifier? #'a)
               #'id]))))]))

(define-syntax (define-dispatch stx)
  (syntax-parse stx
    [(_ () r ...) #'(begin)]
    [(_ name:names [p encoder] ...)
     #:declare p (pred #'name.name)
     #:with id (make-dispatcher (syntax-local-introduce #'(p ...)))
     #:with (e ...) (generate-temporaries #'(encoder ...))
     #'(begin
         (define e encoder) ...
         (define-ctx name.name (id e ...))
         (define-ctx p.name
           (ann (λ (#:ctx ctx . rst)
                  (e ctx rst))
                (-> #:ctx Context Operand * Void)))
         ...
         (define-ctx name.alias name.name) ...
         (provide name.name p.name ... name.alias ...))]))
