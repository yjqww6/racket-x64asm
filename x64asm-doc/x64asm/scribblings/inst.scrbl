#lang scribble/manual
@title[#:tag "inst"]{Instructions}

@defmodule[#:multi (x64asm x64asm/untyped) #:no-declare #:link-target? #f]

@section{Instruction prefix}
@(require (for-label x64asm/private/prefix))
@defform[(rep name args ...)]{
 Call @racket[(name args ...)] with used prefix.
}

@defform[(repe name args ...)]
@defform[(repz name args ...)]
@defform[(repne name args ...)]
@defform[(repnz name args ...)]
@defform[(lock name args ...)]

@section{Instruction list}
@(require (for-label (for-syntax x64asm))
          (for-syntax racket/base))
@(begin-for-syntax 
   (define (get-ids mod)
     (let-values ([(_ p) (module->exports mod)])
       (for*/list ([slot (in-list p)]
                   [slot (in-list (cdr slot))])
         (car slot))
       ))
   (define ids (get-ids 'x64asm/private/inst)))
@(define-syntax (define-insts stx)
   (with-syntax ([(id ...) (datum->syntax #f ids)])
     #'(begin (defproc
                (id [#:ctx ctx Context? (current-context)]
                    [operand (or/c Reg? Mref? Imm? Offset?)]
                    (... ...))
                void?)
              ...)))

@(define-insts)