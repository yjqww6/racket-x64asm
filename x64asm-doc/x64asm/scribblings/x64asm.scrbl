#lang scribble/manual
@(require (for-label x64asm (except-in racket/contract -> case-> ->*)
                     ffi/unsafe
                     (except-in typed/racket/base not -> cast or and))
          racket/runtime-path "sample.rkt")

@title{x64 Assembler}
@author{yjqww6}

@defmodule[x64asm]
@racketmodname[x64asm] is a simple x64 assembler written in Typed Racket.

@(table-of-contents)

@section{Getting Started}
A minimal example of @racket[x64asm] in Typed Racket:

@sample["../examples/start1.txt" k (get-1000)]
@bold{Note.} For untyped racket, use @(racketmodname x64asm/untyped) instead.

A more complicated example about calculating fibonacci numbers:
@sample["../examples/start2.txt" k (fib 40)]

An example without helper macros:
@sample["../examples/start3.txt" k (my-fl+ 100.0 200.0)]

@section{APIs}
@defmodule[x64asm/base #:no-declare]{This module provide apis described here, but not instructions.}

@defmodule[x64asm #:no-declare #:link-target? #f]{This module provide apis described here and instructions.}

@defmodule[x64asm/untyped #:no-declare]{Same as @racketmodname[x64asm], but works in untyped racket.}

@subsection{Core APIs}
@defidform[Assembler]{
 Type of assemblers.
}

@defproc[(make-assembler)
         Assembler?]{
 Creates an assembler.
}

@defparam[current-assembler asm Assembler?]{
 A parameter defines the current assembler.
}

@defproc[(Assembler? [asm any/c])
         boolean?]{
 Tests whether @racket[asm] is an @racket[Assembler].
}

@defproc[(emit-code! [asm Assembler? (current-assembler)]
                     [ctx Context? (assert (current-context))]
                     [ctx* (listof Context?) '()])
         void?]{
 Generates codes from given contexts.
 After calling this function, labels from @racket[ctx] and @racket[ctx*] have their addresses available.
 Memories for generated code are managed by @racket[asm].
 When contexts have mutually referenced labels, they must be submitted to @racket[emit-code!] together.
}

@defproc[(assembler-shutdown-all! [asm Assembler? (current-assembler)])
         void?]{
 Releases memories held by @racket[asm]. Codes generated via @racket[asm] are invalidated.
}

@defidform[Context]{
 Type of contexts. A Context manages a sequence of instruction. Calling a instruction function will append a instruction to that sequence.

 See also @secref{inst}.
}

@defproc[(make-context)
         Context?]{
 Creates a context.
}

@defparam[current-context ctx (or/c #f Context?)]{
 A parameter defines the current context.
}

@defproc[(Context? [ctx any/c])
         boolean?]{
 Tests whether @racket[ctx] is a @racket[Context].
}

@defidform[Label]{Type of labels}
@defproc[(Label? [v any/c]) boolean?]{
 Tests whether @racket[v] is a label.
}
@defform[(label may-id)
         #:grammar
         ((maybe-id (code:line)
                    (code:line id)))]{Creates a label.

 When @racket[id] is given, see also @racket[with-labels].
}


@defproc[(label-addr [l Label?]) exact-nonnegative-integer?]{
 Returns the address of label. This should be called after @racket[emit-code!] is called.
}

@defproc[(:! [#:ctx ctx Context? (assert (current-context))] [l Label?])
         void?]{
 Locates a label in the current code stream of @racket[ctx].
}

@defproc[(data! [#:ctx ctx Context? (assert (current-context))] [datum (or/c bytes? Imm?)] ...)
         void?]{
 Writes custom datum into the code stream of @racket[ctx].
}

@subsection{Instrcution Operands}

@defidform[Size]{Equivalent to @racket[(U 8 16 32 64 80 128 256)].

 @racket[80] and @racket[256] are currently not used.}

@defidform[Scale]{Equivalent to @racket[(U 1 2 4 8)]}

@defidform[Operand]{Equivalent to @racket[(U Imm Mref Offset Reg)]}

@defidform[Reg]{Type of registers}
@defproc[(Reg? [v any/c]) boolean?]{
 Tests whether @racket[v] is a register. See also @secref{reg}
}

@defidform[GPR]{Type of general purpose registers}
@defproc[(GPR? [v any/c]) boolean?]{
 Tests whether @racket[v] is a general purpose register. See also @secref{reg}
}

@defidform[XMM]{Type of xmm registers}
@defproc[(XMM? [v any/c]) boolean?]{
 Tests whether @racket[v] is a xmm register. See also @secref{reg}
}

@defidform[Seg]{Type of segment registers}
@defproc[(Seg? [v any/c]) boolean?]{
 Tests whether @racket[v] is a segment register. See also @secref{reg}
}

@defidform[IP]{Type of instruction pointer registers}
@defproc[(IP? [v any/c]) boolean?]{
 Tests whether @racket[v] is a instruction pointer register. See also @secref{reg}
}

@defidform[Mref]{Type of memory references}
@defproc[(Mref? [v any/c]) boolean?]{
 Tests whether @racket[v] is a memory reference.
}

@defform[#:literals (+ - * :)
         (mref size-expr maybe-seg maybe-sib maybe-disp)
         #:grammar
         [(maybe-seg (code:line)
                     (code:line seg-expr :))
          (maybe-sib (code:line)
                     (code:line base-expr)
                     (code:line base-expr + index-expr * scale-expr)
                     (code:line index-expr * scale-expr))
          (maybe-disp (code:line)
                      (code:line + int/imm-expr)
                      (code:line - int-expr))]]{
 Creates a memory reference.
}

@defidform[Imm]{Type of immediate numbers}
@defproc[(Imm? [v any/c]) boolean?]{
 Tests whether @racket[v] is a immediate number.
}

@defproc*[([(imm8 [num (or/c integer? Label?)] [#:! self! (or/c #f Label?) #f]) Imm?]
           [(imm16 [num (or/c integer? Label?)] [#:! self! (or/c #f Label?) #f]) Imm?]
           [(imm32 [num (or/c integer? Label?)] [#:! self! (or/c #f Label?) #f]) Imm?]
           [(imm64 [num (or/c integer? Label?)] [#:! self! (or/c #f Label?) #f]) Imm?])]{
 Creates an immediate number. When a label is given, its address will be used in generated code stream.
 
 When @racket[self!] is provided, it will point to this immediate number, which may be useful for self-modifying code.
}

@defproc*[([(rel8 [l Label?] [#:! self! (or/c #f Label?) #f]) Imm?]
           [(rel32 [l Label?] [#:! self! (or/c #f Label?) #f]) Imm?])]{
 Creates an immediate number, and its value will be the offset from the next instruction to @racket[l].
 See also @racket[imm8].
}

@defproc[(latent-imm [size (or/c 8 16 32 64)] [proc (-> integer?)]
                     [#:! self! (or/c #f Label?) #f])
         Imm?]{
 Creates an immediate number, but its value is determined by @racket[proc] after labels get their address(enabled for @racket[label-addr]).
}

@defidform[Offset]{Type of memory offsets}
@defproc[(Offset? [v any/c]) boolean?]{
 Tests whether @racket[v] is a memory offset.
}
@defform[#:literals (:)
         (moff size-expr maybe-seg int/imm64-expr)
         #:grammar
         [(maybe-seg (code:line)
                     (code:line seg-expr :))]]{
 Creates a memory offset. Only useful for @racket[mov].
}

@subsection{Helper Macros}

@(require (for-label racket/flonum))
@defform[(define-cast id #:type Type #:ctype ctype-expr)]{
 Defines @racket[id] with type @racket[(Nonnegative-Fixnum -> Type)],
 which is used to cast a memory address to a function.

 @bold{Note: } @racket[require]s from enclosing module would not work inside @racket[ctype-expr] in @racket[typed/racket].
 Use @racket[local-require] instead.
 @racketblock[
 (define-cast flfls
   #:type (FlVector FlVector -> Void)
   #:ctype (let ()
             (local-require racket/flonum)
             (_fun (a b) ::
                   (_pointer = (flvector->cpointer a))
                   (_pointer = (flvector->cpointer b))
                   [_size = (flvector-length b)] -> _void)))]
}

@defform[(with-labels maybe-captured (l ...) body ...)
         #:grammar
         [(maybe-captured (code:line)
                          (code:line #:captured))]]{
 When @racket[#:captured] is not specified, it is equivalent to
 @racketblock[
 (let ()
   (define l (label)) ...
   (let ()
     body
     ...))]

 When @racket[#:captured] is given,
 @racket[(label _id)] syntax is enabled inside @racket[body].
 When @racket[(label _id)] is used, there is no need to define a label before using it.
}

@defform[(λ! cast-expr maybe-labels maybe-asm maybe-capture body ...)
         #:grammar
         [(maybe-labels (code:line)
                        (code:line #:labels (l ...)))
          (maybe-asm (code:line)
                     (code:line #:assembler asm-expr))
          (maybe-capture (code:line)
                         (code:line #:captured))]]{
 Creates a context as @racket[(current-context)], run @racket[body], @racket[emit-code!] via @racket[asm-expr] or @racket[(current-assembler)], and use @racket[cast-expr] to get the returned procedure.

 For @racket[maybe-labels] and @racket[maybe-capture], see @racket[with-labels].
}

@defform[(define-λ! id form ...)]{
 Equivalent to @racketblock[(define id (λ! form ...))].
}

@include-section["registers.scrbl"]
@include-section["inst.scrbl"]
@include-section["expe.scrbl"]
@include-section["advanced.scrbl"]