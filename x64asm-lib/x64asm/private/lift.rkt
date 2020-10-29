#lang racket/base
(require (for-template racket/base)
         racket/match)
  
(provide (all-defined-out))

(define current-lift-context
  (make-parameter #f))

(define (lift expr)
  (match-define (cons ctx b) (current-lift-context))
  (define id
    (internal-definition-context-introduce
     ctx
     (car (generate-temporaries (list expr)))))
  (syntax-local-bind-syntaxes
   (list (syntax-local-introduce id)) #f ctx)
  (set-box! b
            (cons (list id expr)
                  (unbox b)))
  id)

(define (local-expand/capture stx [intdef-ctx '()])
  (define ctx (syntax-local-make-definition-context))
  (define b (box '()))
  (define expanded
    (parameterize ([current-lift-context (cons ctx b)])
      (local-expand stx 'expression null (cons ctx intdef-ctx))))
    
  (with-syntax ([([id expr] ...) (reverse (unbox b))]
                [expanded expanded])
    (with-syntax ([(id ...) (syntax-local-introduce #'(id ...))])
      #'(let ()
          (define id expr)
          ...
          (let ()
            expanded)))))