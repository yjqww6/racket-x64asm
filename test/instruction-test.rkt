#lang typed/racket/base

(module+ test
  (require "../main.rkt"
           (submod "../private/assembler.rkt" debug))
  
  (require (for-syntax racket/base)
           typed/racket/unsafe)
  
  (define-syntax dump!
    (syntax-rules ()
      [(_ body ...)
       (let ([c (make-context)])
         (parameterize ([current-context c])
           (let ()
             body ...)
           (emit-code!)
           (dump-ctx c)))])))


(module+ test
  (require typed/rackunit)
  (check-equal?
   (dump!
    (mov al (moff 8 -1)))
   #"\xa0\xff\xff\xff\xff\xff\xff\xff\xff")
  (check-equal?
   (dump!
    (mov rax (imm64 -1)))
   #"\x48\xb8\xff\xff\xff\xff\xff\xff\xff\xff")
  (check-equal?
   (dump!
    (mov eax (mref 32 rax + rbp * 2 + 9)))
   #"\x8b\x44\x68\x09")
  (check-equal?
   (dump!
    (mov r11 (imm32 -1)))
   #"\x49\xc7\xc3\xff\xff\xff\xff")
  (check-equal?
   (dump!
    (lea rax (mref 64 rdx + 1)))
   #"\x48\x8d\x42\x01")
  (check-equal?
   (dump!
    (unsafe-lea:Gv-Mv rax (mref 64 rdx + 1)))
   #"\x48\x8d\x42\x01")
  
  (check-equal?
   (let ([a 1])
     (dump!
      (lea rax (mref 64 rdx + a))))
   #"\x48\x8d\x42\x01")
  (check-equal?
   (let ([a rdx])
     (dump!
      (lea rax (mref 64 rdx + a * 1))))
   #"\x48\x8d\x04\x12")
  
  (check-equal?
   (dump!
    (bswap r11))
   #"\x49\x0f\xcb")
  (check-equal?
   (dump!
    (bt rax (imm8 1)))
   #"\x48\x0f\xba\xe0\x01")
  (check-equal?
   (dump!
    (call (imm32 0)))
   #"\xe8\x00\x00\x00\x00")
  (check-equal?
   (dump!
    (call rax))
   #"\xff\xd0")
  (check-equal?
   (dump!
    (sal rax (imm8 1)))
   #"\x48\xd1\xe0")
  (check-equal?
   (dump!
    (imul rax rcx (imm8 12)))
   #"\x48\x6b\xc1\x0c")
  (check-equal?
   (dump! (cmpxchg8b (mref 64 rax)))
   #"\x0f\xc7\x08")
  (check-equal?
   (dump! (cmpxchg16b (mref 128 rax)))
   #"\x48\x0f\xc7\x08")
  (check-equal?
   (dump!
    (in al dx)
    (in eax dx)
    (in al (imm8 1))
    (out dx al)
    (out dx eax)
    (out (imm8 1) eax))
   #"\xec\xed\xe4\x01\xee\xef\xe7\x01")

  (check-equal?
   (dump!
    (crc32 eax (mref 8 rax))
    (crc32 rax (mref 8 rax))
    (crc32 r8d (mref 8 rax))
    (crc32 eax (mref 16 rax))
    (crc32 eax (mref 32 rax))
    (crc32 rax (mref 64 rax)))
   (bytes-append
    #"\xf2\x0f\x38\xf0\x00"
    #"\xf2\x48\x0f\x38\xf0\x00"
    #"\xf2\x44\x0f\x38\xf0\x00"
    #"\x66\xf2\x0f\x38\xf1\x00"
    #"\xf2\x0f\x38\xf1\x00"
    #"\xf2\x48\x0f\x38\xf1\x00"
    ))

  (check-equal?
   (dump! (enter (imm16 1) (imm8 2)))
   #"\xc8\x01\x00\x02")
  (check-equal?
   (dump! (rdrand rdi))
   #"\x48\x0f\xc7\xf7")
  (check-equal?
   (dump! (sub rsp (imm8 16)))
   #"\x48\x83\xec\x10")
  (check-equal?
   (dump! (mov r14 (mref 64 rbp + -8)))
   #"\x4c\x8b\x75\xf8")
  (check-equal?
   (dump! (movsxd r11 edx)
          (movsx r11 dx))
   #"\x4c\x63\xda\x4c\x0f\xbf\xda")

  (check-equal?
   (dump!
    (define a (label))
    (define b (label))
    (:! b)
    (cmp rax rbx)
    (jg (rel8 a))
    (nop)
    (nop)
    (:! a)
    (jmp (rel8 b)))
   #"\x48\x39\xd8\x7f\x02\x90\x90\xeb\xf7")

  (define-cast ->intptr
    #:type (-> Integer)
    #:ctype (_fun -> _intptr))
  
  (check-equal?
   ((λ! ->intptr
        (mov rcx (imm32 108))
        (mov rax (imm32 1096))
        (mul rcx)
        (ret)))
   118368)

  (check-equal?
   ((λ! ->intptr #:labels (b)
        (mov rcx (imm32 10))
        (mov rax (imm32 1))
        (:! b)
        (mul rcx)
        (loop (rel8 b))
        (ret)))
   3628800)
  
  (check-equal?
   ((->intptr
     (with-labels #:captured ()
       (define c1 (make-context))
       (define c2 (make-context))
       (parameterize ([current-context c1])
         (:! (label a))
         (xor rax rax)
         (mov rcx (imm32 32))
         (:! (label b))
         (jrcxz (rel8 (label r)))
         (inc rax)
         (dec rcx)
         (jmp (rel32 (label c)))
         (:! (label r))
         (ret))
       (parameterize ([current-context c2])
         (:! (label c))
         (cmp rcx (imm32 0))
         (je (rel32 (label r)))
         (inc rax)
         (dec rcx)
         (jmp (rel32 (label b))))
       (emit-code! (current-assembler) c1 (list c2))
       (label-addr (label a)))))
   32)

  (define-cast int->int
    #:type (Integer -> Integer)
    #:ctype (_fun _int -> _int))
  
  (let ([arg (case (system-type)
               [(windows) ecx]
               [else edi])])
    (define-λ! f int->int
      (popcnt eax arg)
      (ret))
    (check-equal?
     (map f '(2019 65535))
     '(8 16)))

  (check-equal?
   ((λ! ->intptr
        (define b (label))
        (mov rax (imm64 b))
        (call rax)
        (ret)
        (nop)
        (nop)
        (:! b)
        (mov rax (imm32 1096))
        (ret)))
   1096)

  (define-cast p->int
    #:type ((Integer -> Integer) Integer Integer -> Integer)
    #:ctype (_fun (_fun _int -> _int) _int _int -> _int))

  (define-λ! fold p->int #:labels (b c)
    (push rbp)
    (mov rbp rsp)
    (sub rsp (imm8 16))
    (mov (mref 64 rbp - 16) r12)
    (mov (mref 64 rbp - 8) r14)
    (mov r14d esi)
    (mov r12 rdi)
    (mov eax edx)
    (:! b)
    (test r14d r14d)
    (je (rel8 c))
    (mov edi eax)
    (call r12)
    (dec r14d)
    (jmp (rel8 b))
    (:! c)
    (mov r14 (mref 64 rbp - 8))
    (mov r12 (mref 64 rbp - 16))
    (leave)
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
  
  (define-λ! cpuid-has-crc32? ->intptr #:labels (b)
    (mov eax (imm32 1))
    (cpuid)
    (bt ecx (imm8 20))
    (jc (rel8 b))
    (xor rax rax)
    (ret)
    (:! b)
    (mov rax (imm32 1))
    (ret))

  (require (for-label ffi/unsafe))
  
  (define-cast bs->uint
    #:type (Bytes -> Nonnegative-Fixnum)
    #:ctype (_fun (b : _bytes) (_size = (bytes-length b)) -> _uint))
    
  (define-λ! mycrc32c bs->uint #:labels (b c)
    (mov eax (imm32 -1))
    (xor rcx rcx)
    (:! c)
    (cmp rcx rsi)
    (jge (rel8 b))
    (crc32 eax (mref 8 rdi + rcx * 1))
    (inc rcx)
    (jmp (rel8 c))
    (:! b)
    (xor eax (imm32 -1))
    (ret))
  
  (unless (eq? (system-type) 'windows)
    
    (check-equal?
     (fold add1 33 44)
     77)
    
    (let ([src #"test these bytes !!! \xff\xff\xff\xff"]
          [dst (make-bytes 128)])
      (check-equal?
       (let ()
         (memcpy dst src 21)
         (bytes-append (subbytes src 0 21)
                       (make-bytes (- (bytes-length dst)
                                      21))))
       dst))
    
    (when (= (cpuid-has-crc32?) 1)
      (check-equal? (mycrc32c #"abcdefg")
                    #xE627F441)))

  (assembler-shutdown-all!)
  )

(module+ test
  (check-equal?
   (dump! (andps xmm15 xmm2)
          (andps xmm3 xmm2))
   #"\x44\x0f\x54\xfa\x0f\x54\xda")
  (check-equal?
   (dump! (cvtpi2ss xmm1 edi)
          (cvtpi2ss xmm1 r11))
   #"\xf3\x0f\x2a\xcf\xf3\x49\x0f\x2a\xcb")
  
  (check-equal?
   (dump!
    (push rbp)
    (mov rbp rsp)
    (movss (mref 32 rbp - 4) xmm0)
    (movss (mref 32 rbp - 8) xmm1)
    (cvtss2sd xmm1 (mref 32 rbp - 4))
    (cvtss2sd xmm0 (mref 32 rbp - 8))
    (addsd xmm0 xmm0)
    (addsd xmm0 xmm1)
    (cvtsd2ss xmm0 xmm0)
    (pop rbp)
    (ret))
   #"\x55\x48\x89\xe5\xf3\x0f\x11\x45\xfc\xf3\x0f\x11\x4d\xf8\xf3\x0f\x5a\x4d\xfc\xf3\x0f\x5a\x45\xf8\xf2\x0f\x58\xc0\xf2\x0f\x58\xc1\xf2\x0f\x5a\xc0\x5d\xc3")
  
  (check-equal?
   (dump!
    (push rbp)
    (mov rbp rsp)
    (movsd (mref 64 rbp - 8) xmm0)
    (movsd (mref 64 rbp - 16) xmm1)
    (movsd xmm0 (mref 64 rbp - 16))
    (addsd xmm0 xmm0)
    (addsd xmm0 (mref 64 rbp + -8))
    (pop rbp)
    (ret))
   #"\x55\x48\x89\xe5\xf2\x0f\x11\x45\xf8\xf2\x0f\x11\x4d\xf0\xf2\x0f\x10\x45\xf0\xf2\x0f\x58\xc0\xf2\x0f\x58\x45\xf8\x5d\xc3")

  (require math/flonum racket/fixnum "../examples/sysv.rkt")
  
  (unless (eq? (system-type) 'windows)
    (let ([a (inline-build-flvector 9999 fx->fl)]
          [b (inline-build-flvector 9999 (λ (a) 100.0))]
          [c (inline-build-flvector 9999 (λ ([i : Index]) (fl+ (fx->fl i) 100.0)))])
      (flvector-add! a b)
      (check-equal? a c))
    
    (check-equal?
     (vector-sum (build-vector 1000 (ann values (-> Index Index))))
     499500)
    (check-equal?
     (fib 40)
     102334155)
    (check-equal?
     (fib2 40)
     102334155)
    (check-equal?
     (let ([a (make-adder 10)])
       (a 1)
       (a 2)
       (a 3))
     16)
    (check-equal?
     (let-values ([(a b) (make-adder/get 10)])
       (list 
        (a 1)
        (b)
        (a 2)
        (b)
        (a 3)
        (b)))
     '(11 11 13 13 16 16)))
  
  (assembler-shutdown-all!))


(module+ test

  (unsafe-require/typed ffi/unsafe
                        [#:opaque CType ctype?]
                        [_int CType])
  (unsafe-require/typed ffi/cvector
                        [#:opaque CVector cvector?]
                        [cvector (CType Fixnum * -> CVector)])
  
  (define-cast cv->i
    #:type (CVector Index -> Integer)
    #:requires (ffi/cvector)
    #:ctype (_fun _cvector _size -> _int))
  (define-λ! cv-index cv->i
    (mov eax (mref 32
                   (if (eq? (system-type) 'windows)
                       rcx
                       rdi)
                   +
                   (if (eq? (system-type) 'windows)
                       rdx
                       rsi)
                   * 4))
    (ret))
  
  (check-equal? (cv-index (cvector _int 0 0 2019 0 0) 2)
                2019))


(module+ test
  (check-equal?
   (dump!
    (mov al (moff 8 gs : 12))
    (mov rax (moff 64 gs : 12))
    (mov rax (mref 64 fs : rax + rbx * 1 + 5))
    (mov gs (mref 16 rax))
    (mov r11w fs)
    (mov (mref 16 rax) gs)
    (mov r11d fs)
    (mov (mref 16 gs : r11d) fs)
    )
   (bytes-append
    #"\x65\xA0\x0C\x00\x00\x00\x00\x00\x00\x00"
    #"\x65\x48\xA1\x0C\x00\x00\x00\x00\x00\x00\x00"
    #"\x64\x48\x8B\x44\x18\x05"
    #"\x8E\x28"
    #"\x66\x41\x8C\xE3"
    #"\x8C\x28"
    #"\x41\x8C\xE3"
    #"\x67\x65\x41\x8C\x23"))
  (check-equal?
   (dump!
    (push fs)
    (push gs)
    (pop gs)
    (pop fs))
   #"\x0F\xA0\x0F\xA8\x0F\xA9\x0F\xA1")

  (check-equal?
   (dump!
    (mov eax (mref 32 fs : - 4)))
   #"\x64\x8B\x04\x25\xFC\xFF\xFF\xFF"))

(module+ test
  
  (define-λ! f int->int #:captured
    (mov rcx (imm64 (label data)))
    (jmp (mref 64 rcx + rdi * 8))
    (:! (label here))
    (mov eax (imm32 100))
    (ret)
    (:! (label l1))
    (mov eax (imm32 200))
    (ret)
    (:! (label l2))
    (mov eax (imm32 300))
    (ret)

    (:! (label data))
    (data!
     (imm64 (label here))
     (imm64 (label l1))
     (imm64 (label l2))))

  (define-λ! f2 int->int #:captured
    (mov rdx (imm64 (label data)))
    (xor rcx rcx)
    (mov cl (mref 8 rdx + rdi * 1))
    (mov rax (imm64 (label here)))
    (add rax rcx)
    (jmp rax)
    (:! (label here))
    (mov eax (imm32 100))
    (ret)
    (:! (label l1))
    (mov eax (imm32 200))
    (ret)
    (:! (label l2))
    (mov eax (imm32 300))
    (ret)

    (:! (label data))
      
    (define (dist [a : Label] [b : Label])
      (latent-imm 8 (λ ()
                      (- (label-addr b)
                         (label-addr a)))))
    (data!
     (imm8 0)
     (dist (label here) (label l1))
     (dist (label here) (label l2))))
  
  (unless (eq? (system-type) 'windows)
    (check-equal?
     (list (f 0) (f 1) (f 2))
     '(100 200 300))
    (check-equal?
     (list (f2 0) (f2 1) (f2 2))
     '(100 200 300))))

(module+ test
  (check-exn exn:fail?
             (λ ()
               (define a (make-context))
               (ret #:ctx a)
               (emit-code! (current-assembler) a)
               (emit-code! (current-assembler) a)))
  (check-exn exn:fail?
             (λ ()
               (define a (make-context))
               (define l (label))
               (jmp (imm64 l))
               (emit-code! (current-assembler) a)))
  (check-exn exn:fail?
             (λ ()
               (define a (make-context))
               (define l (label))
               (:! l #:ctx a)
               (:! l #:ctx a)))
  (check-exn exn:fail?
             (λ ()
               (define a (make-context))
               (define l (label))
               (jmp (rel8 l))
               (emit-code! (current-assembler) a)))

  (check-exn
   exn:fail?
   (λ ()
     (define c (make-context))
     (define a (label))
     (:! a #:ctx c)
     ;(emit-code! (current-assembler) c)
     (define c2 (make-context))
     (jmp (rel32 a) #:ctx c2)
     (emit-code! (current-assembler) c2)))

  (check-not-exn
   (λ ()
     (define c (make-context))
     (define a (label))
     (:! a #:ctx c)
     (emit-code! (current-assembler) c)
     (define c2 (make-context))
     (jmp (rel32 a) #:ctx c2)
     (emit-code! (current-assembler) c2)))
  )