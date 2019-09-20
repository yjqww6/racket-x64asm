#lang racket
(require "../untyped.rkt" ffi/unsafe)

(define-cast ->int
  #:ctype (_fun -> _int))

(define-λ! test1 ->int #:labels (b)
  (mov eax (imm32 32))
  (lea eax (mref 32 eax + eax * 1))
  (jmp (rel8 b))
  (:! b)
  (ret))

(module+ test
  (require rackunit)
  (check-equal? (test1) 64))