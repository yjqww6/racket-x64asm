#lang racket
(require x64asm/untyped ffi/unsafe)

(define-cast ->int
  #:ctype (_fun -> _int))

(define-λ! test1 ->int #:labels (b)
  (mov eax (imm32 32))
  (lea eax (mref 32 eax + eax * 1))
  (jmp (rel8 b))
  (:! b)
  (ret))

(define-cast bbs->_
  #:type (Bytes Bytes Index -> Void)
  #:ctype (_fun _bytes _bytes _size -> _void))

(define-λ! memcpy bbs->_
  (cld)
  (mov rcx rdx)
  (shr rcx (imm8 3))
  (rep movsq)
  (mov rcx rdx)
  (and rcx (imm32 7))
  (rep movsb)
  (ret))

(module+ test
  (require rackunit)
  (check-equal? (test1) 64))