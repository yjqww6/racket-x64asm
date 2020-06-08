#lang racket/base

(require ffi/unsafe)

(define (lookup a b c d)
  (values
   (get-ffi-obj a #f b (λ () #f))
   (get-ffi-obj c #f d (λ () #f))))

(define (mmap)
  (lookup
   #"mmap"
   (_fun (s) ::
         (_intptr = 0)
         (_size = (max s 1))
         (_int = #x07)
         (_int = (if (eq? (system-type 'os) 'macosx)
                     #x1002
                     #x22))
         (_int = -1)
         (_int64 = 0)
         ->
         [r : _intptr]
         ->
         (if (= r -1)
             (error 'mmap "fail to allocate memory")
             r))

   #"munmap"
   (_fun _uintptr _size -> _int -> (void))))

(define (virtual-alloc)
  (lookup
   #"VirtualAlloc"
   (_fun (s) ::
         (_intptr = 0)
         (_size = (max s 1))
         (_int = #x1000)
         (_int = #x40)
         ->
         [r : _intptr]
         ->
         (if (= r 0)
             (error 'mmap "fail to allocate memory")
             r))
   #"VirtualFree"
   (_fun (p s) :: (p : _uintptr) (_size = 0) (_int = #x8000) -> _int8 -> (void))))

(define-values (allocate-executable-memory free-executable-memory)
  (if (eq? (system-type 'os) 'windows)
      (virtual-alloc)
      (mmap)))

(define copy-executable-memory
  (get-ffi-obj #"memcpy" #f
               (_fun _uintptr _pointer _size -> _void)))

(provide allocate-executable-memory
         free-executable-memory
         copy-executable-memory)
