#lang scribble/manual
@(require racket/runtime-path "sample.rkt"
          (for-label x64asm (except-in racket/contract -> case-> ->*)
                     ffi/unsafe
                     (except-in typed/racket/base not -> cast or and)))
@title[#:tag "adv"]{Advanced Examples}

@section{Self-modifying adder}
@sample["../examples/adder.txt" k
        (define-values (add! get) (make-adder/get 100))
        (add! 10)
        (get)
        (add! 25)
        (get)]

@section{Computed goto}
@sample["../examples/computed.txt" k
        (map f '(0 1 2))
        (map g '(2 1 0))]

@section{Calling C functions}
@sample["../examples/asin.txt" k
        (non-tail 1.0)
        (tail 1.0)]

@section{Error handling via NOP}
Here is an advanced example about error handling via nop in caller side.

The code sums a tree of fixnums, but returns @racket[#f] when a non-fixnum is encountered. 

@sample["../examples/nop.txt" k
        (sum-tree-fixnums 1)
        (sum-tree-fixnums (cons 1 2))
        (sum-tree-fixnums (cons (cons 1 2)
                                (cons 3 4)))
        (sum-tree-fixnums (cons (cons 1 2)
                                (cons 3 'a)))]
