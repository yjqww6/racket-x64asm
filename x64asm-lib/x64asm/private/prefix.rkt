#lang racket/base
(require (for-syntax racket/base syntax/parse)
         "assembler.rkt"
         (only-in typed/racket/base assert))

(define-syntax-rule (define-prefix b prefix ...)
  (begin
    (~@
     (define-syntax (prefix stx)
       (syntax-parse stx
         [(_ (~alt (~optional (~seq #:ctx ctx) #:defaults ([ctx #'(assert (current-context))]))
                   others)
             (... ...))
          #'(let ([c ctx])
              (asm-byte! c b)
              (others (... ...) #:ctx c))]))
     (provide prefix))
    ...))
(define-prefix #xf0 lock)
(define-prefix #xf2 repne repnz)
(define-prefix #xf3 rep repe repz)