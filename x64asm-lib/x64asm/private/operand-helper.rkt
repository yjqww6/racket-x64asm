#lang racket/base
(require (for-syntax racket/base syntax/parse syntax/name
                     syntax/id-table "lift.rkt")
         racket/stxparam "operand.rkt" "registers.rkt"
         (only-in typed/racket/base ann))
(provide label with-labels mref moff)
  
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
         (~and (~or (~literal rip)
                    (~literal eip))
               ip
               (~bind [base #'#f] [is #'#f]))
         (~and (~seq base + index * scale)
               (~bind [is #'(cons index (ann scale Scale))]))
         (~and (~seq index * scale)
               (~bind [base #'#f]
                      [is #'(cons index (ann scale Scale))]))
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
       [(attribute ip)
        #'(Mref size ip #f (or-imm32 disp) seg)]
       [(and (not is?) (not base?))
        #'(Mref size #f #f (or-imm32 disp) seg)]
       [disp?
        #'(Mref size base is (or-imm disp) seg)]
       [else
        #'(Mref size base is #f seg)])]))

(define-syntax (label stx)
  (define (helper stx lifted sym)
    (define id
      (syntax-property
       (datum->syntax lifted (syntax->datum lifted) stx)
       'original-for-check-syntax
       #t))
    (syntax-property lifted sym (list id)))
  (syntax-parse stx
    [(_)
     #`(make-label '#,(syntax-local-infer-name stx))]
    [(_ a)
     (define table (syntax-parameter-value #'current-labels-target))
     (cond
       [(free-id-table-ref table #'a (λ () #f))
        =>
        (λ (lifted)
          (helper #'a lifted 'disappeared-use))]
       [else
        (define lifted (lift #'(make-label 'a)))
        (free-id-table-set! table #'a (syntax-local-introduce lifted))
        (helper #'a lifted 'disappeared-binding)])]))


(define-syntax-parameter current-labels-target #f)

(define-syntax (with-labels-helper stx)
  (syntax-parse stx
    [(_ body ...)
     (local-expand/capture
      #'(let () body ...))]))

(define-syntax (with-labels stx)
  (syntax-parse stx
    [(_ form ...)
     #:when (not (eq? (syntax-local-context) 'expression))
     (syntax/loc stx (#%expression (with-labels form ...)))]
    [(_ (~optional (~and cap #:captured) #:defaults ([cap #'#f]))
        (l:id ...) body ...)
     (cond
       [(syntax-e #'cap)
        #`(let ()
            (define l (label)) ...
            (syntax-parameterize
                ([current-labels-target
                  (make-free-id-table
                   (list (cons #'l #'l) ...))])
              (with-labels-helper body ...))
            )]
       [else
        #'(let ()
            (define l (label)) ...
            (let ()
              body
              ...))])]))