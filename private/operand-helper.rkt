#lang racket/base
(require (for-syntax racket/base syntax/parse syntax/name)
         racket/stxparam "operand.rkt" "registers.rkt"
         (only-in typed/racket/base ann))
(provide label entry with-labels mref moff)
  
(define-syntax (moff stx) 
  (syntax-parse stx #:datum-literals (:) 
    [(_ s (~optional (~seq seg :)
                     #:defaults ([seg #'#f]))
        num) 
     #'(Offset s (or-imm64 num) seg)]))

(define-syntax (mref stx)
  (syntax-parse stx #:datum-literals (+ - * :)
    [(_ size
        (~optional (~seq seg :)
                   #:defaults ([seg #'#f]))
        (~or
         (~and (~seq base + index * scale)
               (~bind [is #'(cons index (ann scale Scale))]))
         (~and (~seq index * scale)
               (~bind [base #'#f] [is #'(cons index (ann scale Scale))]))
         (~and base
               (~bind [is #'#f]))
         (~and (~seq)
               (~bind [base #'#f] [is #'#f])))
        (~optional
         (~or (~and (~seq + d)
                    (~bind [disp #'d]))
              (~and (~seq - d)
                    (~bind [disp #'(- d)])))
         #:defaults ([disp #'#f])))
     #:do [(define base? (syntax-e #'base))
           (define is? (syntax-e #'is))
           (define disp? (syntax-e #'disp))]
     #:when (or base? is? disp?)
     (cond
       [(and (not is?) (not base?))
        #'(Mref size #f #f (or-imm32 disp) seg)]
       [disp?
        #'(Mref size base is (or-imm disp) seg)]
       [else
        #'(Mref size base is #f seg)])]))

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