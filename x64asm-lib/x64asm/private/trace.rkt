#lang typed/racket/base

(provide (all-defined-out))

(: trace-key (Continuation-Mark-Keyof Symbol))
(define trace-key (make-continuation-mark-key 'x64asm))

(define (caller)
  (or (continuation-mark-set-first #f trace-key)
      'unknown))

(define (report-invalid-operands args [c : Symbol (caller)])
  (raise (make-exn:fail:contract
          (format "~a: invalid operands: ~a" c args)
          (current-continuation-marks))))

(define (report-error [fmt : String] . args)
  (raise (make-exn:fail:contract
          (apply format
                 (string-append "~a: " fmt)
                 (caller)
                 args)
          (current-continuation-marks))))