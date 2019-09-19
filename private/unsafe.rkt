#lang racket/base

(require ffi/unsafe)

(define (try . ps)
  (let loop ([ps ps])
    (cond
      [(null? ps) (error 'x64-asm)]
      [else
       (let-values ([(a b) ((car ps))])
         (if (and a b)
             (values a b)
             (loop (cdr ps))))])))

(define (lookup a b c d)
  (values
   (get-ffi-obj a #f b (λ () #f))
   (get-ffi-obj c #f d (λ () #f))))

(define (mmap)
  (lookup
   #"mmap"
   (_fun (_intptr = 0)
         _size
         (_int = #x07)
         (_int = #x22)
         (_int = -1)
         (_int = 0)
         ->
         _uintptr)

   #"munmap"
   (_fun _uintptr _size -> _int -> (void))))

(define (3m)
  (lookup
   #"scheme_malloc_code"
   (_fun _size -> _uintptr)

   #"scheme_free_code"
   (_fun (p s) :: (p : _uintptr) -> _void)))

(define (cs)
  (lookup
   #"S_getmem"
   (_fun _size [_int8 = 0] -> _uintptr)

   #"S_freemem"
   (_fun _uintptr _size -> _void)))

(define (virtual-alloc)
  (lookup
   #"VirtualAlloc"
   (_fun (_intptr = 0) _size (_int = #x1000) (_int = #x40) -> _intptr)
   #"VirtualFree"
   (_fun (p s) :: (p : _uintptr) (_size = 0) (_int = #x8000) -> _int8 -> (void))))

(define-values (allocate-executable-memory free-executable-memory)
  (try 3m cs mmap virtual-alloc))

(define copy-executable-memory
  (get-ffi-obj #"memcpy" #f
               (_fun _uintptr _pointer _size -> _void)))

(provide allocate-executable-memory
         free-executable-memory
         copy-executable-memory)
