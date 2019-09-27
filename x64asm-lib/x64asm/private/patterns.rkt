#lang typed/racket/base

(require "operand.rkt" "registers.rkt"
         racket/match
         (for-syntax racket/base racket/match
                     syntax/parse racket/syntax
                     syntax/stx racket/sequence))

(define-for-syntax atom-preds (make-hasheq))
(define-for-syntax compound-preds (make-hasheq))

(define-syntax define-atom-pred
  (syntax-parser
    [(_ name ? size T)
     #'(begin-for-syntax
         (hash-set! atom-preds 'name (list #'? #'size #'T)))]))

(define-syntax define-compound-pred
  (syntax-parser
    [(_ name C ...)
     #'(begin-for-syntax
         (hash-set! compound-preds 'name (list 'C ...)))]))

(define-atom-pred G GPR? Reg-size GPR)
(define-atom-pred M Mref? Mref-size Mref)
(define-atom-pred I Imm? Imm-size Imm)
(define-atom-pred O Offset? Offset-size Offset)
(define-atom-pred V XMM? Reg-size XMM)

(define-compound-pred E G M)
(define-compound-pred W V M)
(define-compound-pred J I)

(define-for-syntax (get-pred id)
  (hash-ref atom-preds id))

(define-for-syntax (get-preds id)
  (cond
    [(pair? id)
     (apply append (map get-preds id))]
    [(hash-ref compound-preds id (λ () #f))
     =>
     (λ (ls)
       (for/list ([id (in-list ls)])
         (hash-ref atom-preds id)))]
    [else
     (list (hash-ref atom-preds id))]))

(define-for-syntax (build-pred-tree1 case-ls ids ss succ fail)
  ;(displayln succ)
  (match case-ls
    ['() succ]
    [(cons h t)
     (with-syntax ([id (car ids)]
                   [s (car ss)])
       (syntax-parse (get-preds h)
         [([pred size T])
          #`(cond
              [(pred id)
               (define s (size id))
               #,(build-pred-tree1 t (cdr ids) (cdr ss) succ fail)]
              [else #,fail])]
         [([pred size T] ...)
          (if (null? t)
              #`(cond
                  [(pred id)
                   (define s (size id))
                   #,(build-pred-tree1 t (cdr ids) (cdr ss) succ fail)]
                  ...
                  [else #,fail])
              #`(let ([succ (λ ([id : (U T ...)]
                                [s : Size])
                              #,(build-pred-tree1 t (cdr ids) (cdr ss) succ fail))])
                  (cond
                    [(pred id) (succ id (size id))] ...
                    [else #,fail])))]))
     ]))

(define-for-syntax (group-columns cols thunks [no (hasheq)])
  (match cols
    ['() #f]
    [(cons '() col*)
     (group-columns col* (cdr thunks) no)]
    [(cons (cons h t) col*)
     (cond
       [(hash-ref no h (λ () #f))
        (group-columns (cons t col*) thunks no)]
       [else
        (list 'branch h
              (list 'ok
                    (cons (car thunks)
                          (for/list ([col (in-list col*)]
                                     [thunk (in-list (cdr thunks))]
                                     #:when (memq h col))
                            thunk)))
              (group-columns (cons t col*) thunks (hash-set no h #t)))])]))

(define-for-syntax (build-pred-tree cases-ls ids ss succs fail)
  (cond
    [(ormap null? cases-ls) (error 'build-pred-tree)]
    [(null? (cdr cases-ls))
     (build-pred-tree1 (car cases-ls) ids ss (car succs) fail)]
    [else
     (define grouped (group-columns (map car cases-ls)
                                    (map cons (map cdr cases-ls) succs)))
     (let loop ([grouped grouped])
       (match grouped
         [#f fail]
         [(list 'ok (list (cons '() data) ...))
          (build-pred-tree1 '() (cdr ids) (cdr ss)
                            (car data) fail)]
         [(list 'ok data)
          (build-pred-tree (map car data) (cdr ids) (cdr ss)
                           (map cdr data) fail)]
         [(list 'branch (app get-pred (list p size t)) s f)
          #`(if (#,p #,(car ids))
                (let ([#,(car ss) (#,size #,(car ids))])
                  #,(loop s))
                #,(loop f))]))]))

(define-syntax pred-tree
  (let ()
    (define-syntax-class pat
      (pattern pat:id
               #:attr p (datum->syntax
                         #f
                         (hash-ref compound-preds (syntax-e #'pat)
                                   (λ () (list (syntax-e #'pat)))))))
    (syntax-parser
      [(_ (id ...) (s ...)
          [(pat:pat ...) expr] ...)
       (build-pred-tree (syntax->datum #'([pat.p  ...] ...))
                        (syntax->list #'(id ...))
                        (syntax->list #'(s ...))
                        (syntax->list #'(expr ...))
                        #'(error 'fail))])))

(module+ test
  (define (f a b)
    (pred-tree
     (a b) (x y)
     [(G E) (list a b x y)]
     [(E G) (list a b x y)]
     [(V W) (list a b x y)]))
  (for-each
   displayln
   (list
    (f rax rax)
    (f rax (Mref 32 rax #f #f #f))
    (f (Mref 64 rax #f #f #f) rax)
    (f xmm0 xmm1)
    (f xmm0 (Mref 64 rax #f #f #f)))))