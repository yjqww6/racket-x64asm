#lang racket/base
(require (for-syntax racket/base syntax/parse 
                     syntax/id-set)
         "assembler.rkt" "instruction.rkt" "sse.rkt"
         "operand.rkt"
         (prefix-in base: racket/base)
         (only-in typed/racket/base assert))

(define-syntax define-prefix
  (syntax-parser
    [(_ inst:id b:number shuffix:id ...)
     #'(begin
         (define-for-syntax table
           (immutable-free-id-set #'shuffix ...))
         (...
          (define-syntax (prefix stx)
            (syntax-parse stx
              [(_ name:id (~alt (~optional (~seq #:ctx ctx) #:defaults ([ctx #'(assert (current-context))]))
                                args)
                  ...)
               #:when (free-id-set-member? table #'name)
               #'(let ([c ctx])
                   (asm-byte! c b)
                   (inst #:ctx c args ...))]))))]))

(define-syntax define-set
  (syntax-parser
    [(_ name:id id:id ...)
     #'(begin-for-syntax
         (define-syntax-class name
           (pattern (~or (~literal id) ...))))]))

(define-set lock2
  adc add and btc btr bts cmpxchg or sbb sub xadd xor)

(define-set lock1
  cmpxchg8b cmpxchg16b dec inc neg not)

(define-syntax lock
  (syntax-parser
    [(_ name:lock2 ~!
        (~alt (~optional (~seq #:ctx ctx) #:defaults ([ctx #'(assert (current-context))]))
              (~once first:expr)
              (~once second:expr))
        ...)
     #'(let ([c (assert ctx)] [f first] [s second])
         (unless (base:and (Mref? f)
                           (base:not (Mref? s)))
           (error 'lock "~a : invalid operands : ~a ~a"
                  'name f s))
         (asm-byte! c #xf0)
         (name #:ctx c f s))]
    [(_ name:lock1 ~!
        (~alt (~optional (~seq #:ctx ctx) #:defaults ([ctx #'(assert (current-context))]))
              (~once first:expr))
        ...)
     #'(let ([c (assert ctx)] [f first])
         (unless (Mref? f)
           (error 'lock "~a : invalid operands : ~a"
                  'name f))
         (asm-byte! c #xf0)
         (name #:ctx c f))]
    [(_ (~literal xchg) ~!
        (~alt (~optional (~seq #:ctx ctx) #:defaults ([ctx #'(assert (current-context))]))
              (~once first:expr)
              (~once second:expr))
        ...)
     #'(let ([c (assert ctx)] [f first] [s second])
         (unless (base:or (base:and (Mref? f) (base:not (Mref? s)))
                          (base:and (base:not (Mref? f)) (Mref? s)))
           (error 'lock "~a : invalid operands : ~a ~a"
                  'xchg f s))
         (asm-byte! c #xf0)
         (xchg #:ctx c f s))]))

(provide lock)

(define-set rep0
  insb insw insd lodsb lodsw lodsd lodsq
  outsb outsw outsd stosb stosw stosd stosq
  movsb movsw movsd movsq)

(define-syntax rep
  (syntax-parser
    [(_ name:rep0 (~optional (~seq #:ctx ctx) #:defaults ([ctx #'(assert (current-context))])))
     #'(let ([c (assert ctx)])
         (asm-byte! c #xf3)
         (name #:ctx c))]))
(provide rep)


(define-set repx0
  cmpsb cmpsw cmpsd cmpsq
  scasb scasw scasd scasq)

(define-syntax define-p0
  (syntax-parser
    [(_  id:id code:number)
     #'(begin
         (define-syntax id
           (syntax-parser
             [(_ name:repx0 (~optional (~seq #:ctx ctx) #:defaults ([ctx #'(assert (current-context))])))
              #'(let ([c (assert ctx)])
                  (asm-byte! c code)
                  (name #:ctx c))]))
         (provide id))]))

(define-p0 repz #xf3)
(define-p0 repe #xf3)
(define-p0 repnz #xf2)
(define-p0 repne #xf2)