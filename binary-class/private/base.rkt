#lang racket/base
(require racket/class)
(provide binary
         binary?
         read-value
         write-value
         binary<%>
         values-box)

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
     (for ([v (rest-values)]) (send value write out))]))

(define binary<%> (interface () read write))