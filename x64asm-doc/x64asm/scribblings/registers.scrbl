#lang scribble/manual
@title[#:tag "reg"]{Registers}

@(define-syntax-rule (define-registers id ...)
   (begin (defthing id Reg?) ...))

@(define-registers
   al ah ax eax rax
   cl ch cx ecx rcx
   dl dh dx edx rdx
   bl bh bx ebx rbx
   spl sp esp rsp
   bpl bp ebp rbp
   sil si esi rsi
   dil di edi rdi
   r8b r8w r8d r8
   r9b r9w r9d r9
   r10b r10w r10d r10
   r11b r11w r11d r11
   r12b r12w r12d r12
   r13b r13w r13d r13
   r14b r14w r14d r14
   r15b r15w r15d r15
   xmm0 xmm1 xmm2 xmm3
   xmm4 xmm5 xmm6 xmm7
   xmm8 xmm9 xmm10 xmm11
   xmm12 xmm13 xmm14 xmm15
   fs gs)

