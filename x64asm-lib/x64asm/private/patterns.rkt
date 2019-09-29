#lang racket/base

(require (for-syntax racket/base racket/match syntax/parse))
(provide (all-defined-out) (for-syntax (all-defined-out)))

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

(define-for-syntax (get-types id)
  (map (λ (x) (caddr (hash-ref atom-preds x)))
       (hash-ref compound-preds id
                 (λ () (list id)))))

(define-for-syntax (get-pred id)
  (hash-ref atom-preds id))

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
    [else
     (define grouped (group-columns (map car cases-ls)
                                    (map cons (map cdr cases-ls)
                                         succs)))
     (let loop ([grouped grouped])
       (match grouped
         [#f fail]
         [(list 'ok (list (cons '() data) ...))
          (syntax-case data ()
            [([guard expr] ...)
             #`(cond
                 [guard expr] ...
                 [else #,fail])])]
         [(list 'ok (list (cons p data) ...))
          (build-pred-tree p (cdr ids) (cdr ss)
                           data fail)]
         [(list 'branch (app get-pred (list p size t)) s f)
          #`(if (#,p #,(car ids))
                (let ([#,(car ss) (#,size #,(car ids))])
                  #,(loop s))
                #,(loop f))]))]))

(define-syntax pred-tree
  (let ()
    (define-syntax-class pat
      (pattern pat:id
               #:attr p
               (datum->syntax
                #f
                (hash-ref compound-preds
                          (syntax-e #'pat)
                          (λ () (list (syntax-e #'pat)))))))
    (syntax-parser
      [(_ (id ...) (s ...) err
          [(pat:pat ...) guard expr] ...)
       (build-pred-tree (syntax->datum #'([pat.p  ...] ...))
                        (syntax->list #'(id ...))
                        (syntax->list #'(s ...))
                        (syntax->list #'([guard expr] ...))
                        #'err)])))


#;
(module+ test
  (define (f a b)
    (pred-tree
     (a b) (x y)
     [(G E) #t (list a b x y)]
     [(E G) #t (list a b x y)]
     [(V W) #t (list a b x y)]))
  (for-each
   displayln
   (list
    (f rax rax)
    (f rax (Mref 32 rax #f #f #f))
    (f (Mref 64 rax #f #f #f) rax)
    (f xmm0 xmm1)
    (f xmm0 (Mref 64 rax #f #f #f)))))