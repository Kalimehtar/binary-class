#lang racket/base
(provide binary
         binary?
         read-value
         write-value
         read-object
         binary<%>
         define-binary-class)

(require (for-syntax racket/base syntax/parse racket/syntax syntax/id-table)
         racket/class)

(struct binary (read write))

(define (read-value type in)
  ((binary-read type) in))

(define (write-value type out value)
  ((binary-write type) out value))

(define (read-object binary-class in . args)
  (send (apply make-object binary-class args) read in))


(define (copy-object old new)
  (for ([f (field-names old)])
    (dynamic-set-field! f new (dynamic-get-field f old))))

(define binary<%> (interface () read write))

(begin-for-syntax 
  (define fields (make-free-id-table))
  (define (get-fields id) (free-id-table-ref fields id null))
  (define (save-fields! id value) (free-id-table-set! fields id value) (void)))

(define-syntax (define-binary-class stx)
  (define (not-null? x)
    (not (free-identifier=? x #'_)))
  (define (make-reader id+val)
    (with-syntax ([(FNAME FTYPE) id+val])
      (if (not-null? #'FNAME)
          #'(set! FNAME ((binary-read FTYPE) in))
          #'((binary-read FTYPE) in))))
  (define (make-writer id+val)
    (with-syntax ([(FNAME FTYPE) id+val])
      (if (not-null? #'FNAME)
          #'((binary-write FTYPE) out FNAME)
          #'((binary-write FTYPE) out #f))))
  (syntax-parse stx
    [(_ NAME ((FNAME FTYPE) ...) maybe-options ...)
     #'(define-binary-class NAME object% ((FNAME FTYPE) ...) maybe-options ...)]
    [(_ NAME:id SUPER:expr ((FNAME:id FTYPE:expr) ...) (~optional (~seq #:dispatch DISPATCH:expr)))
     (with-syntax* ([(SUPER-FIELD ...) (datum->syntax stx (get-fields #'SUPER))]
                    [(NOT-NULL-FIELD ...) 
                     (filter not-null?
                             (syntax->list #'(FNAME ...)))]
                    [(READER ...)
                     (map make-reader (syntax->list #'((FNAME FTYPE) ...)))]
                    [(WRITER ...)
                     (map make-writer (syntax->list #'((FNAME FTYPE) ...)))]
                    [(ALL-FIELD ...) (append (syntax->list #'(SUPER-FIELD ...))
                                             (syntax->list #'(NOT-NULL-FIELD ...)))]
                    [RETURN (if (attribute DISPATCH)
                                #'(let ([obj (new DISPATCH)])
                                    (copy-object this obj)
                                    (send obj read in)
                                    obj)
                                #'this)])
       #'(begin
           (define NAME 
             (if (implementation? SUPER binary<%>)
                 (class SUPER
                   (super-new)
                   (inherit-field SUPER-FIELD ...)
                   (field (NOT-NULL-FIELD #f) ...)
                   (define/override (read in [skip-dispatch? #f])
                     (super read in #t)
                     READER ...
                     (if skip-dispatch? this RETURN))
                   (define/override (write out)
                     (super write out)
                     WRITER ...))
                 (class* SUPER (binary<%>)
                   (super-new)
                   (inherit-field SUPER-FIELD ...)
                   (field (NOT-NULL-FIELD #f) ...)
                   (define/public (read in [skip-dispatch? #f])
                     READER ...
                     (if skip-dispatch? this RETURN))
                   (define/public (write out)
                     WRITER ...))))
           (define-syntaxes ()
             (begin0 (values) (save-fields! #'NAME (list 'ALL-FIELD ...))))))]))


(module+ test
  (require rackunit)
  (define u1 (binary (λ (in) 'ok) (λ (value out) (void))))
  (define u2 (binary (λ (in) 'ok2) (λ (value out) (void))))
  (test-begin
   (define-binary-class test ((a u1) (_ u1) (_ u1)))
   (define-binary-class test2 test ((b u2)))
   (define tmp (read-object test2 1))
   (check-eq? (get-field a tmp) 'ok)
   (check-eq? (get-field b tmp) 'ok2))
  (test-begin
   (define-binary-class disp ((a read-val)) #:dispatch (if (= a 1) disp2 disp3))
   (define read-val (binary (λ (in) in) (λ (value out) (void))))
   (define return-val (λ (x) (binary (λ (in) x) (λ (value out) (void)))))
   (define-binary-class disp2 disp ((b u1)))
   (define-binary-class disp3 disp ((b u2)))
   (check-eq? (get-field b (read-object disp 1)) 'ok)
   (check-eq? (get-field b (read-object disp 2)) 'ok2)
   (define-binary-class disp4 disp ((b (return-val (+ a 1)))))
   (check-eqv? (get-field b (read-object disp4 2)) 3)
   (check-eqv? (get-field b (read-object disp4 8)) 9)))
