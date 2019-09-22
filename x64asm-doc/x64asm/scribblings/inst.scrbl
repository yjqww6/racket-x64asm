#lang scribble/manual
@title[#:tag "inst"]{Instructions}

@section{Instruction prefix}
@(require (for-label x64asm/private/prefix))
@defform[(rep form ...)]{
 Call @racket[(form ...)] with used prefix.
}

@defform[(repe form ...)]
@defform[(repz form ...)]
@defform[(repne form ...)]
@defform[(repnz form ...)]
@defform[(lock form ...)]

@section{Instruction list}
@(require (for-label (for-syntax x64asm)) (for-syntax racket/base))
@(begin-for-syntax 
   (define (get-ids mod)
     (let-values ([(_ p) (module->exports mod)])
       (for*/list ([slot (in-list p)]
                   [slot (in-list (cdr slot))]
                   #:when (not (regexp-match? #rx"unsafe-.*" (symbol->string (car slot)))))
         (car slot))
       ))
   (define ids (append (get-ids 'x64asm/private/instruction)
                       (get-ids 'x64asm/private/sse))))
@(define-syntax (define-insts stx)
   (with-syntax ([(id ...) (datum->syntax #f ids)])
     #'(begin (defproc
                (id [#:ctx ctx Context? (current-context)]
                    [operand (or/c Reg? Mref? Imm? Offset?)]
                    (... ...))
                void?)
              ...)))

@(define-insts)