#lang racket/base
(require (for-syntax racket/base syntax/parse syntax/name)
         racket/stxparam "operand.rkt")
(provide label entry with-labels mref moff)
  
(define-syntax-rule (moff s num)
  (Mref s #f #f (Immediate 64 num)))
  
(define-syntax (mref stx)
  (syntax-parse stx #:literals (+ - *)
    [(_ size a + b * c)
     #'(Mref size a (cons b c) #f)]
    [(_ size a + b * c + d)
     #'(Mref size a (cons b c) (or-imm d))]
    [(_ size a + b * c - d)
     #'(Mref size a (cons b c) (or-imm (- d)))]
    [(_ size b * c + d)
     #'(Mref size #f (cons b c) (or-imm d))]
    [(_ size b * c - d)
     #'(Mref size #f (cons b c) (or-imm (- d)))]
    [(_ size a + d:number)
     #'(Mref size a #f (or-imm d))]
    [(_ size a - d:number)
     #'(Mref size a #f (or-imm (- d)))]
    [(_ size a + d)
     #'(let ([ts size]
             [ta a]
             [td d])
         (if (Reg? td)
             (Mref ts ta (cons td 1) #f)
             (Mref ts ta #f (or-imm td))))]
    [(_ size b * c)
     #'(Mref size #f (cons b c) #f)]
    [(_ size a)
     #'(Mref size a #f #f)]))

(define-syntax (label stx)
  (syntax-parse stx
    [(_)
     #`(make-label '#,(syntax-local-infer-name stx) #t)]
    [(_ a)
     (define table (syntax-parameter-value #'current-labels-target))
     (hash-ref! table (syntax-e #'a)
                (λ () (syntax-local-lift-expression #'(make-label 'a #t))))]))

(define-syntax (entry stx)
  (syntax-parse stx
    [(_)
     #`(make-label '#,(syntax-local-infer-name stx) #f)]
    [(_ a)
     (define table (syntax-parameter-value #'current-labels-target))
     (hash-ref! table (syntax-e #'a)
                (λ () (syntax-local-lift-expression #'(make-label 'a #f))))]))


(define-syntax-parameter current-labels-target #f)

(define-syntax (with-labels-helper stx)
  (syntax-parse stx
    [(_ body ...)
     (local-expand/capture-lifts
      #'(let () body ...)
      'expression
      '())]))

(define-syntax (with-labels stx)
  (syntax-parse stx
    [(_ (~optional (~and cap #:captured) #:defaults ([cap #'#f]))
        ((~alt (~seq #:entry e:id) l:id) ...) body ...)
     (cond
       [(syntax-e #'cap)
        #`(let ()
            (define e (entry)) ...
            (define l (label)) ...
            (syntax-parameterize
                ([current-labels-target
                  (make-hasheq
                   (list (cons 'l #'l) ...
                         (cons 'e #'e) ...))])
              (with-labels-helper body ...))
            )]
       [else
        #'(let ()
            (define e (entry)) ...
            (define l (label)) ...
            (let ()
              body
              ...))])]))