#lang racket/base
(require racket/contract/base racket/class)

(module unsafe racket/base
  (provide binary
           binary?
           read-value
           write-value
           #;read-object
           binary<%>
           define-binary-class)
  
  (require (for-syntax racket/base syntax/parse racket/syntax syntax/id-table)
           racket/class)
  
  (struct binary (read write))
  
  (define (read-value type in . args)
    (if (binary? type)
        ((binary-read type) in)
        (send (apply make-object type args) read in args)))
      
  (define (write-value type out value)
    (if (binary? type)
        ((binary-write type) out value)
        (send value write out)))
    
  (define (copy-object old new)
    (for ([f (field-names old)])
      (dynamic-set-field! f new (dynamic-get-field f old))))
  
  (define binary<%> (interface () read write))
  
  (begin-for-syntax 
    (define fields (make-free-id-table))
    (define (get-fields id) 
      (if (identifier? id)
          (free-id-table-ref fields id null)
          null))
    (define (save-fields! id value) (free-id-table-set! fields id value) (void)))
  
  (define-syntax (define-binary-class stx)
    (define (not-null? x)
      (not (free-identifier=? x #'_)))
    (define (make-reader id+val)
      (with-syntax ([(FNAME FTYPE ARG ...) id+val])
        (if (not-null? #'FNAME)
            #'(set! FNAME (read-value FTYPE in ARG ...))
            #'(read-value FTYPE in ARG ...))))
    (define (make-writer id+val)
      (with-syntax ([(FNAME FTYPE ARG ...) id+val])
        (if (not-null? #'FNAME)
            #'(write-value FTYPE out FNAME)
            #'(write-value FTYPE out #f))))
    (syntax-parse stx
                  [(_ NAME ((FNAME FTYPE ARG ...) ...) BODY ...)
                   #'(define-binary-class NAME object% ((FNAME FTYPE ARG ...) ...) BODY ...)]
                  [(_ NAME:id SUPER:expr ((FNAME:id FTYPE:expr ARG ...) ...)
                      (~optional (~seq #:dispatch DISPATCH:expr))
                      BODY ...)
                   (with-syntax* ([(SUPER-FIELD ...) (datum->syntax stx (get-fields #'SUPER))]
                                  [(NOT-NULL-FIELD ...) 
                                   (filter not-null?
                                           (syntax->list #'(FNAME ...)))]
                                  [(READER ...)
                                   (map make-reader (syntax->list #'((FNAME FTYPE ARG ...) ...)))]
                                  [(WRITER ...)
                                   (map make-writer (syntax->list #'((FNAME FTYPE ARG ...) ...)))]
                                  [(ALL-FIELD ...) (append (syntax->list #'(SUPER-FIELD ...))
                                                           (syntax->list #'(NOT-NULL-FIELD ...)))]
                                  [RETURN (if (attribute DISPATCH)
                                              #'(let ([obj (apply make-object DISPATCH args)])
                                                  (copy-object this obj)
                                                  (send obj read in args #f this%)
                                                  obj)
                                              #'this)])
                                 #'(begin
                                     (define a-super SUPER)
                                     (define NAME
                                       (if (implementation? a-super binary<%>)
                                           (class a-super
                                             (super-new)
                                             (inherit-field SUPER-FIELD ...)
                                             (field (NOT-NULL-FIELD #f) ...)
                                             (define/override (read in 
                                                                    [args null] 
                                                                    [skip-dispatch? #f]
                                                                    [skip-super-class #f])
                                               (unless (eq? skip-super-class this%) 
                                                 (super read in args #t skip-super-class)
                                                 READER ...
                                                 (if skip-dispatch? this RETURN)))
                                             (define/override (write out)
                                               (super write out)
                                               WRITER ...
                                               (void))
                                             BODY ...)
                                           (class* a-super (binary<%>)
                                             (super-new)
                                             (inherit-field SUPER-FIELD ...)
                                             (field (NOT-NULL-FIELD #f) ...)
                                             (define/public (read in 
                                                                  [args null]
                                                                  [skip-dispatch? #f]
                                                                  [skip-super-class #f])
                                               (unless (eq? skip-super-class this%)
                                                 READER ...
                                                 (if skip-dispatch? this RETURN)))
                                             (define/public (write out)
                                               WRITER ...
                                               (void))
                                             BODY ...)))
                                     (define-syntaxes ()
                                       (begin0 (values) 
                                               (save-fields! #'NAME (list 'ALL-FIELD ...))))))])))

(require 'unsafe)

(provide/contract
 [binary (-> (-> input-port? any)
             (-> output-port? any/c any)
             binary?)]
 [binary? (-> any/c boolean?)]
 [write-value (-> (or/c binary? (implementation?/c binary<%>)) output-port? any/c any)]
 [read-value (->* ((or/c binary? (implementation?/c binary<%>)) input-port?)
                   #:rest list?
                   any)]
 [binary<%> interface?])
(provide define-binary-class)

(module+ test
  (require rackunit)
  (displayln "Testing...")
  (define u1 (binary (λ (in) 'ok) (λ (value out) (void))))
  (define u2 (binary (λ (in) 'ok2) (λ (value out) (void))))
  (test-begin
   (define-binary-class test ((a u1) (_ u1) (_ u1)))
   (define-binary-class test2 test ((b u2)))
   (define tmp (read-value test2 1))
   (check-eq? (get-field a tmp) 'ok)
   (check-eq? (get-field b tmp) 'ok2))
  (test-begin
   (define-binary-class disp ((a read-val)) #:dispatch (if (= a 1) disp2 disp3))
   (define read-val (binary (λ (in) in) (λ (value out) (void))))
   (define return-val (λ (x) (binary (λ (in) x) (λ (value out) (void)))))
   (define-binary-class disp2 disp ((b u1)))
   (define-binary-class disp3 disp ((b u2)))
   (check-eq? (get-field b (read-value disp 1)) 'ok)
   (check-eq? (get-field b (read-value disp 2)) 'ok2)
   (define-binary-class disp4 disp ((b (return-val (+ a 1)))))
   (check-eqv? (get-field b (read-value disp4 2)) 3)
   (check-eqv? (get-field b (read-value disp4 8)) 9)))

(module* safe #f
  (provide/contract
   [binary (-> (-> input-port? any/c)
               (-> output-port? any/c void?)
               binary?)]
   [binary? (-> any/c boolean?)]
   [write-value (-> (or/c binary? (implementation?/c binary<%>)) output-port? any/c void?)]
   [read-value (->i ([binary-class (or/c binary? (implementation?/c binary<%>))]
                      [port input-port?])
                     #:rest [args list?]
                     [result (binary-class) (is-a?/c binary-class)])]
   [binary<%> interface?])
  (provide define-binary-class))
