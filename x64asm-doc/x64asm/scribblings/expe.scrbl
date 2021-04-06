#lang scribble/manual
@(require scribble/examples
          (for-label x64asm/unsafe x64asm/well-typed))

@title{Experimental features}
@section{Unsafe variants of Instructions}
@defmodule[x64asm/unsafe #:no-declare]

This module provide unsafe variants of instructions that skip argument dispatching and some checking.
@examples[#:eval (make-base-eval #:lang 'typed/racket/base)
          (require x64asm/unsafe)
          (:print-type mov:Eb-Gb)]

@section{Well-typed variants of Instructions}
@defmodule[x64asm/well-typed #:no-declare]

This module provide more precisely typed variants of instructions.
@examples[#:eval (make-base-eval #:lang 'typed/racket/base)
          (require x64asm/well-typed)
          (:print-type mov)]

Noted that these functions no longer accept a keyword context argument, you must provide it explicitly.

@section{Instructions that accept a list}
@defmodule[x64asm/apply #:no-declare]

Since @racket[apply] cannot be used with keyword arguments in Type Racket,
this module provide variants of instructions that accept a list of operands.
@examples[#:eval (make-base-eval #:lang 'typed/racket/base)
          (require x64asm/apply)
          (:print-type mov)]
