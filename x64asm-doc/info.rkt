#lang info
(define collection 'multi)
(define deps '("base"))
(define build-deps '("x64asm-lib" "scribble-lib" "racket-doc"
                                  "typed-racket-doc"
                                  "typed-racket-lib"))
(define update-implies '("x64asm-lib"))
(define pkg-desc "a simple x64 assembler written in typed/racket")
(define version "0.2")
(define pkg-authors '(yjqww6))
