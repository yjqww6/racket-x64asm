#lang typed/racket/base

(require "assembler.rkt" "operand.rkt" "registers.rkt"
         racket/match racket/fixnum)
(require/typed "unsafe.rkt"
               [allocate-executable-memory (Nonnegative-Fixnum -> Nonnegative-Fixnum)]
               [free-executable-memory (Nonnegative-Fixnum Nonnegative-Fixnum -> Void)]
               [copy-executable-memory (Nonnegative-Fixnum Bytes Nonnegative-Fixnum -> Void)])
(provide emit-code! find-entry assembler-shutdown-all!)

(define (ensure-no-unresolve [asm : Assembler] [ctxs : (Listof Context)])
  (let loop : Void
    ([required : (HashTable Label True) (hasheq)]
     [labels (for/hasheq : (HashTable Label True)
               ([k (in-hash-keys (Assembler-global-labels asm))])
               (values k #t))]
     [ls ctxs])
    (match ls
      ['()
       (define s
         (for/fold : (HashTable Label True)
           ([h required])
           ([k (in-hash-keys labels)])
           (hash-remove h k)))
       (unless (hash-empty? s)
         (error 'emit-code! "unresolved references ~a" s))]
      [(cons c ls)
       (unless (fx= (Context-addr c) 0)
         (error 'emit-code! "This Context has been emited: ~a" c))
       (loop (for/fold ([h required])
                       ([k (in-hash-keys (Context-label-required c))])
               (hash-set h (Reloc-Cell-label k) #t))
             (for/fold ([h labels])
                       ([k (in-hash-keys (Context-local-labels c))])
               (hash-set h k #t))
             ls)])))

(: emit-code! (->* ()
                   (Assembler (Option Context) (Listof Context))
                   Void))
(define (emit-code! [asm (current-assembler)]
                    [c (current-context)]
                    [c* '()])
  (define ctxs (cons (assert c) c*))
  (ensure-no-unresolve asm ctxs)
  (define allocated
    : (Listof (Pairof Nonnegative-Fixnum Nonnegative-Fixnum))
    '())
  (dynamic-wind
   void
   (λ ()
     (for ([ctx (in-list ctxs)])
       (define addr
         (allocate-executable-memory
          (Context-offset ctx)))
       (set! allocated (cons (cons addr (Context-offset ctx))
                             allocated))
       (set-Context-addr! ctx addr))
     
     (define labels
       (for*/hasheq : (HashTable Label Nonnegative-Fixnum)
         ([ctx (in-list ctxs)]
          [(label off) (in-hash (Context-local-labels ctx))])
         (values label (fx+ (Context-addr ctx) off))))

     (for* ([ctx (in-list ctxs)]
            [(reloc start)
             (in-hash (Context-label-required ctx))])
       (match reloc
         [(Reloc-Cell label off size rel?)
          (define p (hash-ref labels label))
          (define num
            (cond
              [rel?
               (fx- p (+ (Context-addr ctx) start))]
              [else
               p]))
          ;may not fit
          (define b (integer->integer-bytes num (fxquotient size 8) #t))
          (bytes-copy! (Context-buf ctx) off b)]))

     (define (finder [l : Label])
       (hash-ref labels l))

     (for* ([ctx (in-list ctxs)]
            [reloc (in-list (Context-custom-relocs ctx))])
       (match reloc
         [(Reloc-Custom size off proc)
          (define num (proc finder))
          (define b (integer->integer-bytes num (fxquotient size 8) #t))
          (bytes-copy! (Context-buf ctx) off b)]))
     
     (set-Assembler-pages! asm (append allocated (Assembler-pages asm)))
     (set! allocated '())

     (for ([(l o) (in-hash labels)]
           #:when (not (Label-local? l)))
       (hash-set! (Assembler-global-labels asm)
                  l o))

     (for ([ctx (in-list ctxs)])
       (copy-executable-memory (Context-addr ctx)
                               (Context-buf ctx)
                               (Context-offset ctx)))
     )
   (λ () (for ([a (in-list allocated)])
           (free-executable-memory (car a) (cdr a)))))
  (void))

(define (find-entry [label : Label] [asm : Assembler (current-assembler)])
  (hash-ref (Assembler-global-labels asm)
            label))

(define (assembler-shutdown-all! [asm : Assembler (current-assembler)])
  (for ([p (in-list (Assembler-pages asm))])
    (free-executable-memory (car p) (cdr p)))
  (set-Assembler-pages! asm '())
  (set-Assembler-global-labels! asm (make-hasheq)))