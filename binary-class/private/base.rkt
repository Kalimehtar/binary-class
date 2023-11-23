#lang racket/base
(require racket/class)
(provide binary
         binary?
         read-value
         write-value
         binary<%>
         values-box
         symbolic-binary)

(struct binary (read write))

(struct values-box (value))

(define (read-value type in . args)
  (cond    
    [(binary? type) ((binary-read type) in)]
    [(implementation? type binary<%>) (send (apply make-object type args) read in args)]
    [(values-box? type) (apply values (values-box-value type))]
    [else type]))

(define (write-value type out value . rest-values)
  (cond 
    [(binary? type) (apply (binary-write type) out value rest-values)]
    [(implementation? type binary<%>) 
     (send value write out)
     (for ([v (in-list rest-values)]) (send value write out))]))

(define binary<%> (interface () read write))

(define-syntax symbolic-binary
  (syntax-rules ()
    ((_ type (sym val) ...)
     (let ((t type))
       (binary
        (lambda (in)
          (let* ((value (read-value t in))
                 (symbol (cond
                           ((equal? value val) 'sym)
                           ...
                           (else (raise-argument-error
                                  'symbolic-binary "invalid value" value)))))
            symbol))
        (lambda (out symbol)
          (let ((value (cond
                         ((eqv? symbol 'sym) val)
                         ...
                         (else (raise-argument-error
                                'symbolic-binary "invalid symbol" symbol)))))
            (write-value t value))))))))

