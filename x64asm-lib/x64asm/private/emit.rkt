#lang typed/racket/base

(require "assembler.rkt" "operand.rkt" "registers.rkt"
         racket/match racket/fixnum)
(require/typed "unsafe.rkt"
               [allocate-executable-memory (Nonnegative-Fixnum -> Nonnegative-Fixnum)]
               [free-executable-memory (Nonnegative-Fixnum Nonnegative-Fixnum -> Void)]
               [copy-executable-memory (Nonnegative-Fixnum Bytes Nonnegative-Fixnum -> Void)])
(provide emit-code! assembler-shutdown-all!)

(: emit-code! (->* ()
                   (Assembler (Option Context) (Listof Context))
                   Void))
(define (emit-code! [asm (current-assembler)]
                    [c (current-context)]
                    [c* '()])
  (define ctxs (cons (assert c) c*))
  (define allocated
    : (Listof (Pairof Nonnegative-Fixnum Nonnegative-Fixnum))
    '())
  (dynamic-wind
   void
   (λ ()
     (for ([ctx (in-list ctxs)])
       (when (not (fx= (Context-addr ctx) 0))
         (error 'emit-code! "This context has been emited! : ~a" ctx))
       (define addr
         (allocate-executable-memory
          (Context-offset ctx)))
       (set! allocated (cons (cons addr (Context-offset ctx))
                             allocated))
       (set-Context-addr! ctx addr))
     
     (for*
       ([ctx (in-list ctxs)]
        [(label off) (in-hash (Context-local-labels ctx))])
       (set-box! (Label-assigned? label) (fx+ (Context-addr ctx) off)))

     (for* ([ctx (in-list ctxs)]
            [(reloc start)
             (in-hash (Context-label-required ctx))])
       (match reloc
         [(Reloc-Cell label off size rel?)
          (define p (label-addr label))
          (define num
            (cond
              [rel?
               (fx- p (+ (Context-addr ctx) start))]
              [else
               p]))
          ;may not fit
          (define b (integer->integer-bytes num (fxquotient size 8) #t))
          (bytes-copy! (Context-buf ctx) off b)]))

     (for* ([ctx (in-list ctxs)]
            [reloc (in-list (Context-custom-relocs ctx))])
       (match reloc
         [(Reloc-Custom size off proc)
          (define num (proc))
          (define b (integer->integer-bytes num (fxquotient size 8) #t))
          (bytes-copy! (Context-buf ctx) off b)]))
     
     (set-Assembler-pages! asm (append allocated (Assembler-pages asm)))
     (set! allocated '())

     (for ([ctx (in-list ctxs)])
       (copy-executable-memory (Context-addr ctx)
                               (Context-buf ctx)
                               (Context-offset ctx)))
     )
   (λ () (for ([a (in-list allocated)])
           (free-executable-memory (car a) (cdr a)))))
  (void))

(define (assembler-shutdown-all! [asm : Assembler (current-assembler)])
  (for ([p (in-list (Assembler-pages asm))])
    (free-executable-memory (car p) (cdr p)))
  (set-Assembler-pages! asm '()))