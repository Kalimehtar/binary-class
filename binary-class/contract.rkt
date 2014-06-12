#lang racket/base
(require racket/class racket/contract)
(provide binary-class/c 
         (contract-out [binary-integer/c (->* (exact-integer?) (exact-integer?) flat-contract?)]))
(define-syntax-rule (binary-class/c CLASS BODY ...)
  (class/c
   BODY ...
   [read (->m input-port? (is-a?/c CLASS))]
   [write (->m output-port? void?)]))

(define (binary-integer/c bytes [bits-per-byte 8])
  (integer-in 0 (sub1 (expt 2 (* bytes bits-per-byte)))))
