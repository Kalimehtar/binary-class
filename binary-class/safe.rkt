#lang racket/base
(require racket/contract/base racket/class "unsafe.rkt")
(provide/contract
 [binary (-> (-> input-port? any/c)
             (-> output-port? any/c void?)
             binary?)]
 [binary? (-> any/c boolean?)]
 [read-value (-> binary? input-port? any/c)]
 [write-value (-> binary? output-port? any/c void?)]
 [read-object (->i ([binary-class (implementation?/c binary<%>)]
                    [port input-port?])
                   #:rest [args list?]
                   [result (binary-class) (is-a?/c binary-class)])]
 [binary<%> interface?])
(provide define-binary-class)