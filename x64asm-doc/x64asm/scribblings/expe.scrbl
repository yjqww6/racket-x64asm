#lang scribble/manual
@(require scribble/examples
          (for-label x64asm/unsafe x64asm/well-typed)
          (for-label (only-in x64asm current-context)))

@title{Experimental features}
@section{Unsafe version of Instructions}
@defmodule[x64asm/unsafe]

This module provide unsafe version of instructions that skip argument dispatching and some checking.
@examples[#:eval (make-base-eval #:lang 'typed/racket/base)
          (require x64asm/unsafe)
          (:print-type mov:Eb-Gb)
          (:type #:verbose Encoder)]

@section{Well-typed version of Instructions}
@defmodule[x64asm/well-typed]

This module provide more precisely typed version of instructions.
@examples[#:eval (make-base-eval #:lang 'typed/racket/base)
          (require x64asm/well-typed)
          (:print-type mov)]

Noted that these function no longer accept a keyword context argument, use @racket[current-context].

@section{Instructions that accept a list}
@defmodule[x64asm/apply]

Since @racket[apply] cannot be used with keyword arguments in Type Racket,
this module provide another version of instructions that accept a list of operands.
@examples[#:eval (make-base-eval #:lang 'typed/racket/base)
          (require x64asm/apply)
          (:print-type mov)]
