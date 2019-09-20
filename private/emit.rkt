#lang typed/racket/base

(require "assembler.rkt" "operand.rkt" "registers.rkt"
         racket/match racket/fixnum)
(require/typed "unsafe.rkt"
               [allocate-executable-memory (Nonnegative-Fixnum -> Nonnegative-Fixnum)]
               [free-executable-memory (Nonnegative-Fixnum Nonnegative-Fixnum -> Void)]
               [copy-executable-memory (Nonnegative-Fixnum Bytes Nonnegative-Fixnum -> Void)])
(provide emit-code! find-entry reset-assembler!)

(define (ensure-no-unresolve [asm : Assembler])
  (let loop : Void
    ([required : (HashTable Label True) (hasheq)]
     [labels (for/hasheq : (HashTable Label True)
               ([k (in-hash-keys (Assembler-global-labels asm))])
               (values k #t))]
     [ls (Assembler-contexts asm)])
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
       (loop (for/fold ([h required])
                       ([k (in-hash-keys (Context-label-required c))])
               (hash-set h (Reloc-Cell-label k) #t))
             (for/fold ([h labels])
                       ([k (in-hash-keys (Context-local-labels c))])
               (hash-set h k #t))
             ls)])))

(define (emit-code! [asm : Assembler (current-assembler)])
  (ensure-no-unresolve asm)
  (define allocated
    : (Listof (Pairof Nonnegative-Fixnum Nonnegative-Fixnum))
    '())
  (dynamic-wind
   void
   (λ ()
     (define contexts
       (for/list : (Listof (Pairof Context Nonnegative-Fixnum))
         ([ctx (in-list (Assembler-contexts asm))])
         (define addr
           (allocate-executable-memory
            (Context-offset ctx)))
         (set! allocated (cons (cons addr (Context-offset ctx))
                               allocated))
         (cons ctx addr)))
     (define labels
       (for*/hasheq : (HashTable Label Nonnegative-Fixnum)
         ([ctx (in-list contexts)]
          [(label off) (in-hash (Context-local-labels (car ctx)))])
         (values label (fx+ (cdr ctx) off))))

     (for* ([ctx (in-list contexts)]
            [(reloc start)
             (in-hash (Context-label-required (car ctx)))])
       (match reloc
         [(Reloc-Cell label off size rel?)
          (define p (hash-ref labels label))
          (define num
            (cond
              [rel?
               (fx- p (+ (cdr ctx) start))]
              [else
               p]))
          ;may not fit
          (define b (integer->integer-bytes num (fxquotient size 8) #t))
          (bytes-copy! (Context-buf (car ctx)) off b)]))
     (set-Assembler-pages! asm (append allocated (Assembler-pages asm)))
     (set! allocated '())

     (for ([(l o) (in-hash labels)]
           #:when (not (Label-local? l)))
       (hash-set! (Assembler-global-labels asm)
                  l o))

     (for ([ctx (in-list contexts)])
       (copy-executable-memory (cdr ctx)
                               (Context-buf (car ctx))
                               (Context-offset (car ctx))))
     
     (set-Assembler-contexts! asm '()))
   (λ () (for ([a (in-list allocated)])
           (free-executable-memory (car a) (cdr a)))))
  (void))

(define (find-entry [label : Label] [asm : Assembler (current-assembler)])
  (hash-ref (Assembler-global-labels asm)
            label))

(define (reset-assembler! [asm : Assembler (current-assembler)])
  (for ([p (in-list (Assembler-pages asm))])
    (free-executable-memory (car p) (cdr p)))
  (set-Assembler-pages! asm '())
  (set-Assembler-global-labels! asm (make-hasheq)))