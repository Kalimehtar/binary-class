#lang racket/base
(require racket/contract/base racket/class "unsafe.rkt")
(provide/contract
 [binary (-> (-> input-port? any)
             (-> output-port? any/c any)
             any)]
 [binary? (-> any/c any)]
 [read-value (-> binary? input-port? any)]
 [write-value (-> binary? output-port? any/c any)]
 [read-object (->* ((implementation?/c binary<%>) input-port?)
                   #:rest list?
                   any)]
 [binary<%> interface?])
(provide define-binary-class)
