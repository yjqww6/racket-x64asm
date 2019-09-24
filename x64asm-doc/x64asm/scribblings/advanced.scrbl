#lang scribble/manual
@(require racket/runtime-path "sample.rkt"
          (for-label x64asm (except-in racket/contract -> case-> ->*)
                     ffi/unsafe
                     (except-in typed/racket/base not -> cast or and)))
@title[#:tag "adv"]{Advanced Examples}

@section{Self-modifying adder}
@sample["../examples/adder.rkt" k
        (define-values (add! get) (make-adder/get 100))
        (add! 10)
        (get)
        (add! 20)
        (get)]

@section{Computed goto}
@sample["../examples/data.rkt" k
        (map f '(0 1 2))
        (map g '(0 1 2))]

@section{Error handling via NOP}
Here is an advanced example for error handling via nop in caller side.

The code sums a tree of fixnums, and returns @racket[#f] when a non-fixnum is encountered. 

@sample["../examples/advanced.rkt" k
        (sum-tree-fixnums 1)
        (sum-tree-fixnums (cons 1 2))
        (sum-tree-fixnums (cons (cons 1 2)
                                (cons 3 4)))
        (sum-tree-fixnums (cons (cons 1 2)
                                (cons 3 'a)))]
