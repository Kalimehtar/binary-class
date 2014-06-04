#lang racket/base
(require racket/contract/base racket/class "unsafe.rkt")
(provide/contract
 [binary (-> (-> input-port? any)
             (-> output-port? any/c void?)
             binary?)]
 [binary? (-> any/c boolean?)]
 [read-value (-> binary? input-port? any)]
 [write-value (-> binary? output-port? any/c void?)]
 [read-object (->* ((implementation?/c binary<%>) input-port?)
                   #:rest list?
                   (instanceof/c (implementation?/c binary<%>)))]
 [binary<%> interface?])
(provide define-binary-class)
