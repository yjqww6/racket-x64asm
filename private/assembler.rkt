#lang typed/racket/base
(provide (all-defined-out))
(require "registers.rkt" "operand.rkt"
         racket/match racket/fixnum racket/stxparam
         (for-syntax racket/base racket/syntax))

(struct Assembler ([global-labels : (HashTable Label Nonnegative-Fixnum)]
                   [contexts : (Listof Context)]
                   [pages : (Listof (Pairof Nonnegative-Fixnum Nonnegative-Fixnum))])
  #:mutable)

(define (make-assembler)
  (Assembler (make-hasheq) '() '()))

(define current-assembler (make-parameter (make-assembler)))

(define current-context : (Parameterof (Option Context))
  (make-parameter #f))

(struct Reloc-Cell ([label : Label] [off : Nonnegative-Fixnum] [size : Size] [rel? : Boolean])
  #:transparent)
(struct Reloc-Custom ([size : Size] [off : Nonnegative-Fixnum] [proc : ((Label -> Nonnegative-Fixnum) -> Integer)]))

(struct Context ([inst-cache : Bytes]
                 [inst-size : Nonnegative-Fixnum]
                 [inst-relocs : (Listof Reloc-Cell)]
                 [buf : Bytes]
                 [offset : Nonnegative-Fixnum]
                 [asm : Assembler]
                 [custom-relocs : (Listof Reloc-Custom)]
                 [local-labels : (HashTable Label Nonnegative-Fixnum)]
                 [label-required : (HashTable Reloc-Cell Nonnegative-Fixnum)])
  #:mutable)

(define (make-context [asm : Assembler (current-assembler)])
  (define ctx
    (Context (make-bytes 15) 0 '() (make-bytes 256) 0 asm '() (make-hasheq) (make-hasheq)))
  (set-Assembler-contexts! asm (cons ctx (Assembler-contexts asm)))
  ctx)

(define (enlarge-buf [ctx : Context]
                     [size : Nonnegative-Fixnum])
  (let ([old (Context-buf ctx)])
    (cond
      [(> size (bytes-length old))
       (define b (make-bytes (max (fx* 2 (bytes-length old))
                                  size)))
       (bytes-copy! b 0 old)
       (set-Context-buf! ctx b)
       b]
      [else old])))

(define (finish-instruction! [ctx : Context])
  (define len (Context-inst-size ctx))
  (define off (Context-offset ctx))
  (define buf
    (enlarge-buf ctx (fx+ len off)))
  (bytes-copy! buf off (Context-inst-cache ctx) 0 len)
  (set-Context-offset! ctx (fx+ off len))
  (set-Context-inst-size! ctx 0)
  (for ([reloc (in-list (Context-inst-relocs ctx))])
    (hash-set! (Context-label-required ctx)
               reloc (Context-offset ctx)))
  (set-Context-inst-relocs! ctx '()))

(define (asm-byte! [ctx : Context] [b : Byte])
  (define cache (Context-inst-cache ctx))
  (define start (Context-inst-size ctx))
  (bytes-set! cache start b)
  (set-Context-inst-size! ctx (fx+ start 1)))

(define-syntax (asm-byte*! stx)
  (syntax-case stx ()
    [(_ c b ...)
     (with-syntax* ([len (length (syntax->list #'(b ...)))]
                    [(i ...) (build-list (syntax-e #'len) values)])
       #'(let ([ctx c])
           (define cache (Context-inst-cache ctx))
           (define start (Context-inst-size ctx))
           (bytes-set! cache (fx+ start i) (ann b Byte))
           ...
           (set-Context-inst-size! ctx (fx+ start len))))]))

(define (asm-bytes! [ctx : Context] [b : Bytes])
  (define cache (Context-inst-cache ctx))
  (define start (Context-inst-size ctx))
  (bytes-copy! cache start b)
  (set-Context-inst-size! ctx (fx+ start (bytes-length b))))

(define (asm-label! [ctx : Context] [l : Label])
  (assert (not (unbox (Label-assigned? l))))
  (set-box! (Label-assigned? l) #t)
  (hash-set! (Context-local-labels ctx) l (Context-offset ctx)))

(define (asm-imm! [ctx : Context] [imm : Imm])
  (define self (Imm-self imm))
  (when self
    (hash-set! (Context-local-labels ctx) self
               (fx+ (Context-offset ctx)
                    (Context-inst-size ctx))))
  (match imm
    [(Immediate 8 _ num)
     #:when (<= -128 num 255)
     (asm-byte! ctx (fxand num #xff))]
    [(Immediate 16 _ num)
     #:when (<= (- (expt 2 15)) num (- (expt 2 16) 1))
     (asm-byte*! ctx
                 (fxand num #xff)
                 (fxand (fxrshift num 8) #xff))]
    [(Immediate 32 _ num)
     #:when (<= (- (expt 2 31)) num (- (expt 2 32) 1))
     (asm-byte*! ctx
                 (fxand num #xff)
                 (fxand (fxrshift num 8) #xff)
                 (fxand (fxrshift num 16) #xff)
                 (fxand (fxrshift num 24) #xff))]
    [(Immediate 64 _ num)
     (cond
       [(< num 0)
        (asm-bytes! ctx (integer->integer-bytes num 8 #t))]
       [else
        (asm-bytes! ctx (integer->integer-bytes num 8 #f))])]
    [(Relocate:Label size _ label rel?)
     (define cell
       (Reloc-Cell label (fx+ (Context-offset ctx)
                              (Context-inst-size ctx))
                   size rel?))
     (set-Context-inst-relocs!
      ctx
      (cons cell (Context-inst-relocs ctx)))
     (asm-bytes! ctx (make-bytes (fxrshift size 3)))]
    [(Relocate:Custom size _ proc)
     (set-Context-custom-relocs!
      ctx
      (cons (Reloc-Custom size
                          (fx+ (Context-offset ctx)
                               (Context-inst-size ctx))
                          proc)
            (Context-custom-relocs ctx)))
     (set-Context-inst-size! ctx
                             (fx+ (fxrshift size 3)
                                  (Context-inst-size ctx)))]))

(define (:! [l : Label] #:ctx [ctx : (Option Context) (current-context)])
  (assert ctx)
  (asm-label! ctx l))

(: data! ([#:ctx (Option Context)] (U Bytes Imm) * -> Void))
(define (data! #:ctx [ctx (current-context)] . datum)
  (assert ctx)
  (let ()
    (define len
      (for/fold ([len : Nonnegative-Fixnum 0])
                ([d (in-list datum)])
        (cond
          [(bytes? d) (fx+ len (bytes-length d))]
          [else
           (fx+ len (Imm-size d))])))
    (define buf (enlarge-buf ctx (fx+ (Context-offset ctx) len)))
    (for ([d (in-list datum)])
      (cond
        [(bytes? d)
         (bytes-copy! buf (Context-offset ctx) d)
         (set-Context-offset! ctx
                              (fx+ (Context-offset ctx) (bytes-length d)))]
        [else
         (asm-imm! ctx d)
         (finish-instruction! ctx)])))
  )

(module+ debug
  (define (dump-ctx [ctx : Context])
    (subbytes (Context-buf ctx) 0 (Context-offset ctx)))
  (provide dump-ctx))
