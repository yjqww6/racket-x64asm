#lang racket/base
(require syntax/parse/define (for-syntax racket/base))
(provide (all-defined-out) (for-syntax get-types))

(module pred racket/base
  (require racket/base racket/match (for-template racket/base))
  (provide (all-defined-out))

  (struct Atom (pred size type) #:authentic)
  (define atom-preds (make-hasheq))
  (define compound-preds (make-hasheq))

  (define (get-pred id)
    (hash-ref atom-preds id))
  
  (define (get-types id)
    (map (λ (x) (Atom-type (hash-ref atom-preds x)))
         (hash-ref compound-preds id
                   (λ () (list id)))))

  (struct Branch (test then else) #:authentic)
  (struct Leaf (datum) #:authentic)
  
  (define (build-column cols datum)
    (let f ([cols cols] [datum datum] [no (hasheq)])
      (match cols
        ['() #f]
        [(cons '() col*)
         (f col* (cdr datum) no)]
        [(cons (cons h t) col*)
         (cond
           [(hash-ref no h (λ () #f))
            (f (cons t col*) datum no)]
           [else
            (Branch h (Leaf (cons (car datum)
                                  (for/list ([col (in-list col*)]
                                             [datum (in-list (cdr datum))]
                                             #:when (memq h col))
                                    datum)))
                    (f (cons t col*) datum (hash-set no h #t)))])])))

  (define (build-pred-tree rows ids ss clauses fail)
    (cond
      [(ormap null? rows) (error 'build-pred-tree)]
      [else
       (let f ([rows rows] [ids ids] [ss ss] [clauses clauses])
         (define built (build-column (map car rows)
                                     (map cons (map cdr rows)
                                          clauses)))
         (let loop ([built built])
           (match built
             [#f fail]
             [(Leaf (list (cons '() data) ...))
              (syntax-case data ()
                [(clauses ...)
                 #`(cond
                     clauses ...
                     [else #,fail])])]
             [(Leaf (list (cons p data) ...))
              (f p (cdr ids) (cdr ss) data)]
             [(Branch (app get-pred (Atom p size t)) s f)
              #`(if (#,p #,(car ids))
                    (let ([#,(car ss) (#,size #,(car ids))])
                      #,(loop s))
                    #,(loop f))])))])))

(require (for-syntax 'pred))

(define-syntax define-atom-pred
  (syntax-parser
    [(_ name ? size T)
     #'(begin-for-syntax
         (hash-set! atom-preds 'name (Atom #'? #'size #'T)))]))

(define-syntax define-compound-pred
  (syntax-parser
    [(_ name C ...)
     #'(begin-for-syntax
         (hash-set! compound-preds 'name (list 'C ...)))]))

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
      [(_ (id ...) (size:id ...) err
          [(pat:pat ...) guard:expr expr] ...)
       (build-pred-tree (syntax->datum #'([pat.p  ...] ...))
                        (syntax->list #'(id ...))
                        (syntax->list #'(size ...))
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