#lang scribble/manual
@(require scribble/examples racket/file racket/runtime-path
          (for-label x64asm (except-in racket/contract -> case-> ->*)
                     ffi/unsafe
                     (except-in typed/racket/base not -> cast or and)))
@title[#:tag "adv"]{Advanced Example}

@(define ev (make-base-eval #:lang 'racket
                            '(require racket/enter)))

Here is an advanced example summing a tree of fixnums 

@(define-runtime-path code "../examples/advanced.rkt")
@codeblock[(file->string code)]

@(ev `(enter! (file ,(path->string code))))
@examples[#:eval ev
          (sum-tree-fixnums 1)
          (sum-tree-fixnums (cons 1 2))
          (sum-tree-fixnums (cons (cons 1 2)
                                  (cons 3 4)))
          (sum-tree-fixnums (cons (cons 1 2)
                                  (cons 3 'a)))]
