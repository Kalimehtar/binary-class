#lang racket/base
(require "private/base.rkt" racket/class
         (for-syntax racket syntax/parse racket/syntax syntax/id-table))
(provide define-binary-class)

(begin-for-syntax 
  (define-syntax-class ids
    (pattern (id* ...+) #:attr ids #'(id* ...))
    (pattern id #:attr ids #'(id))))

(define-syntax (define-binary-class stx)
  (syntax-parse stx
    [(_ NAME ((FNAME FTYPE ARG ...) ...) BODY ...)
     #'(define-binary-class NAME object% ((FNAME FTYPE ARG ...) ...) BODY ...)]
    [(_ NAME:id SUPER:expr ((FNAME:ids FTYPE:expr ARG:expr ...) ...)
        (~optional (~seq #:dispatch DISPATCH:expr))
        BODY ...)
     (with-syntax* ([(SUPER-FIELD ...) (datum->syntax stx (get-fields #'SUPER))]
                    [(NOT-NULL-FIELD ...) 
                     (filter not-null?
                             (apply append (map syntax->list (syntax->list #'(FNAME.ids ...)))))]
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
                                #'this)]
                    [(BEGIN ...) 
                     #'((super-new)
                        (inherit-field SUPER-FIELD ...)
                        (field (NOT-NULL-FIELD #f) ...))]
                    [READ/public (read-template #'define/public #'NAME #'RETURN #'(READER ...))]
                    [READ/override (read-template #'define/override 
                                                  #'NAME 
                                                  #'RETURN 
                                                  #'((super read in args #t skip-super-class) 
                                                     READER ...))])
                   #'(begin
                       (define a-super SUPER)
                       (define NAME
                         (if (implementation? a-super binary<%>)
                             (class a-super
                               BEGIN ...
                               READ/override
                               (define/override (write out)
                                 (super write out)
                                 WRITER ...
                                 (void))
                               BODY ...)
                             (class* a-super (binary<%>)
                               BEGIN ...
                               READ/public
                               (define/public (write out)
                                 WRITER ...
                                 (void))
                               BODY ...)))
                       (define-syntaxes ()
                         (begin0 (values) 
                                 (save-fields! #'NAME (list 'ALL-FIELD ...))))))]))

(define-syntax-rule (values->maybe-list FTYPE)
  (let ([l (call-with-values (λ () FTYPE) list)])
    (if (null? (cdr l)) (car l) (values-box l))))

(define-for-syntax (make-reader id+val)
  (with-syntax ([(FNAME FTYPE ARG ...) id+val])
    (syntax-case #'FNAME ()
      [(NAME ...)
       (with-syntax ([(LOCAL ...) (generate-temporaries #'(NAME ...))])
         #'(let-values ([(LOCAL ...) (read-value (values->maybe-list FTYPE) in ARG ...)])
             (set* NAME LOCAL) ...))]
      [NAME #'(set* FNAME (read-value FTYPE in ARG ...))])))

(define-syntax (set* stx)
  (syntax-case stx ()
    [(_ VAR VALUE) (not-null? #'VAR) #'(set! VAR VALUE)]
    [(_ VAR VALUE)                   #'(begin VALUE (void))]))

(define-for-syntax (make-writer id+val)
  (with-syntax ([(FNAME FTYPE ARG ...) id+val])
    (syntax-case #'FNAME ()
      [(NAME ...)
       (with-syntax ([(NAME* ...) (map not-null? (syntax->list #'(NAME ...)))])
         #`(write-value FTYPE out NAME* ...))]
      [NAME (not-null? #'NAME) #'(write-value FTYPE out NAME)]
      [NAME                    #'(write-value FTYPE out #f)])))

(define-for-syntax (read-template stx-DEFINE stx-NAME stx-RETURN stx-READER)
  (with-syntax ([DEFINE stx-DEFINE]
                [NAME stx-NAME]
                [RETURN stx-RETURN]
                [(READER ...) stx-READER])
    #'(DEFINE (read in 
                    [args null] 
                    [skip-dispatch? #f]
                    [skip-super-class #f])
              (unless (eq? skip-super-class NAME) 
                READER ...
                (if skip-dispatch? this RETURN)))))

(define-for-syntax (not-null? x)
  (if (free-identifier=? x #'_) #f x))

(define (copy-object old new)
  (for ([f (field-names old)])
    (dynamic-set-field! f new (dynamic-get-field f old))))

(define-for-syntax fields (make-free-id-table))
  
(define-for-syntax (get-fields id) 
  (if (identifier? id)
      (free-id-table-ref fields id null)
      null))
  
(define-for-syntax (save-fields! id value) 
  (free-id-table-set! fields id value) (void))
