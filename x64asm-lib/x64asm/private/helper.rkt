#lang typed/racket/base
(require racket/match
         (for-syntax racket/base syntax/parse
                     syntax/id-table))

(provide define-struct-match)

(begin-for-syntax
  (define-syntax-class kid
    (pattern
     id:id
     #:attr kw (datum->syntax #f (string->keyword (symbol->string (syntax-e #'id)))))))

(define-syntax (define-struct-match stx)
  (syntax-parse stx
    [(_ name:id s:id field:kid ...)
     #'(define-match-expander name
         (λ (stx)
           (syntax-parse stx
             [(_ (~alt (~optional (~seq field.kw field) #:defaults ([field #'_]))
                       ...)
                 (... ...))
              #'(s field ...)]
             )))]))