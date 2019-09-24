#lang scribble/manual
@(require (for-label x64asm (except-in racket/contract -> case-> ->*)
                     ffi/unsafe
                     (except-in typed/racket/base not -> cast or and))
           scribble/examples)

@title{x64 Assembler}
@author{yjqww6}

@defmodule[x64asm]
@racketmodname[x64asm] is a simple x64 assembler written in Typed Racket.

@(table-of-contents)

@section{Getting Started}

@(define ev (make-base-eval #:lang 'typed/racket/base))

A minimal example for @racket[x64asm] in Typed Racket would be:
@examples[#:eval ev
          (require x64asm)
          (define-cast ->int
            #:type (-> Integer)
            #:ctype (_fun -> _int))
          (define-λ! get-1000 ->int
            (mov rax (imm32 1000))
            (ret))
          (get-1000)]
@bold{Note.} For untyped racket, use @racket[(require x64asm/untyped)] instead.

Now there is a more complicated example for calculating fibonacci numbers:
@examples[#:eval ev
          (define-cast int->int
            #:type (Integer -> Integer)
            #:ctype (_fun _int64 -> _int64))
          
          (define-λ! fib int->int #:labels (start l1 l2 l3)
            (define arg0 (if (eq? (system-type)'windows) rcx rdi))
            (:! start)
            (push rbp)
            (mov rbp rsp)
            (sub rsp (imm8 16))
            
            (cmp arg0 (imm8 2))
            (jg (rel8 l1))
            (mov rax (imm32 1))
            (leave)
            (ret)
            
            (:! l1)
            (sub arg0 (imm8 1))
            (mov (mref 64 rbp - 8) arg0)
            (call (rel32 start))
            (mov (mref 64 rbp - 16) rax)
            (mov arg0 (mref 64 rbp - 8))
            (sub arg0 (imm8 1))
            (call (rel32 start))
            (add rax (mref 64 rbp - 16))
            (leave)
            (ret)
            )
          (fib 40)]

An example without helper macros is
@examples[#:eval ev
          (define-cast dd->d
            #:type (Flonum Flonum -> Flonum)
            #:ctype (_fun _double _double -> _double))
          (define my-fl+
            (parameterize ([current-context (make-context)])
              (define entry (label))
              (:! entry)
              (addsd xmm0 xmm1)
              (ret)
            
              (emit-code!)
              (dd->d (label-addr entry))))
          (my-fl+ 100.0 200.0)]

@section{APIs}

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
                     [ctx Context? (current-context)]
                     [ctx* (listof Context?) '()])
         void?]{
 Generates codes from given contexts.
 After calling this function, labels from @racket[ctx] and @racket[ctx*] have their addresses available.
 Memories for generated code are managed by @racket[asm].
 When contexts have mutually referenced labels, they must be submitted to @racket[emit-code!] together.
}

@defproc[(assembler-shutdown-all! [asm Assembler? (current-assembler)])
         void?]{
 Release memories held by @racket[asm]. Codes generated via @racket[asm] are invalidated.
}

@defidform[Context]{
 Type of contexts. A Context manages a sequence of instruction. Calling a instruction function will append a instruction to that sequence.

 See also @secref{inst}.
}

@defproc[(make-context)
         Context?]{
 Creates a context.
}

@defparam[current-context ctx Context?]{
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
 Get the address of label. This should be called after @racket[emit-code!] is called.
}

@defproc[(:! [#:ctx ctx Context? (current-context)] [l Label?])
         void?]{
 Locates a label in the current code stream of @racket[ctx].
}

@defproc[(data! [#:ctx ctx Context? (current-context)] [datum (or/c bytes? Imm?)])
         void?]{
 Write custom datum into the code stream of @racket[ctx].
 @examples[#:eval ev
           (define-λ! f int->int #:captured
             (define arg0 (if (eq? (system-type)'windows) rcx rdi))
             (mov rax (imm64 (label data)))
             (jmp (mref 64 rax + arg0 * 8))
             (:! (label here))
             (mov eax (imm32 100))
             (ret)
             (:! (label l1))
             (mov eax (imm32 200))
             (ret)
             (:! (label l2))
             (mov eax (imm32 300))
             (ret)

             (:! (label data))
             (data!
              (imm64 (label here))
              (imm64 (label l1))
              (imm64 (label l2))))
           (map f '(0 1 2))]
}

@subsection{Instrcution Operands}

@defidform[Size]{Equivalent to @racket[(U 8 16 32 64 80 128 256)].

@racket[80] and @racket[256] are currently not used.}

@defidform[Scale]{Equivalent to @racket[(U 1 2 4 8)]}

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
 @examples[#:eval ev
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

 @bold{Experimental: } When @racket[#:captured] is given,
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
@include-section["advanced.scrbl"]