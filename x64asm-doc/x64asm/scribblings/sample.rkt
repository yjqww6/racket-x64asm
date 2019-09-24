#lang scribble/manual
@require[scribble/examples racket/runtime-path racket/file]
@provide[sample]

@(define-syntax-rule (my-example path datum ...)
   (let ()
     (define ev (make-base-eval #:lang 'racket/base
                                '(require racket/enter)))
     (ev `(enter! (file ,(path->string path))))
     (examples #:eval ev datum ...)))
@(define-syntax-rule (sample path k expr ...)
   (begin
     @(define-runtime-path p path)
     @codeblock[#:context #'k (file->string p)]
     @my-example[p expr ...])
   )
