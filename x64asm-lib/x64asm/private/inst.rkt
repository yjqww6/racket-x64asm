#lang racket/base
(require (for-syntax racket/base racket/match racket/syntax
                     syntax/parse)
         (only-in typed/racket/base assert)
         (only-in "assembler.rkt" current-context)
         (submod "instruction.rkt" base)
         (submod "sse.rkt" base)
         (submod "instruction.rkt" procedure)
         (submod "sse.rkt" procedure))

(define-syntax (define-proxy stx)
  (syntax-parse stx
    [(_ name:id)
     #:with tmp (generate-temporary #'name)
     #:with proc (format-id #'name "proc:~a" #'name)
     #'(...
        (begin
          (define-syntax tmp
            (syntax-parser
              [(_ (~alt
                   (~optional (~seq #:ctx c)
                              #:defaults ([c #'(assert (current-context))]))
                   arg)
                  ...)
               #'(name c arg ...)]
              [n:id #'proc]))
          (provide (rename-out [tmp name]))))]))

(define-syntax define-proxies
  (syntax-parser
    [(_ mod)
     (match/values
      (module->exports (syntax->datum #'mod))
      [((cons (list 0 (cons id _) ...) _) _)
       (with-syntax ([(id ...)
                      (map (λ (x) (datum->syntax #'mod x)) id)])
         #'(begin (define-proxy id)
                  ...))])]))

(define-proxies (submod "sse.rkt" base))
(define-proxies (submod "instruction.rkt" base))

