#lang racket/base
(require racket/class racket/contract)
(provide binary-class/c 
         (contract-out [binary-integer/c (->* (exact-integer?) (exact-integer?) flat-contract?)]
                       [u1? flat-contract?]
                       [u2? flat-contract?]
                       [u3? flat-contract?]
                       [u4? flat-contract?]
                       [iso-8859-1? flat-contract?]
                       [ucs-2? flat-contract?]
                       [iso-8859-1-len/c (-> real? flat-contract?)]
                       [ucs-2-len/c (-> real? flat-contract?)]))
(define-syntax-rule (binary-class/c CLASS BODY ...)
  (class/c
   BODY ...
   [read (->m input-port? (is-a?/c CLASS))]
   [write (->m output-port? void?)]))

(define (binary-integer/c bytes [bits-per-byte 8])
  (integer-in 0 (sub1 (expt 2 (* bytes bits-per-byte)))))

(define u1? (binary-integer/c 1))
(define u2? (binary-integer/c 2))
(define u3? (binary-integer/c 3))
(define u4? (binary-integer/c 4))

(define iso-8859-1?
  (and/c string? 
         (flat-named-contract 'iso-8859-1
                              (λ (s)
                                (for/and ([c (in-string s)])
                                  (< (char->integer c) 256))))))

(define ucs-2?
  (and/c string? 
         (flat-named-contract 'ucs-2
                              (λ (s)
                                (for/and ([c (in-string s)])
                                  (< (char->integer c) 65536))))))

(define (iso-8859-1-len/c length)
  (and/c iso-8859-1? (string-len/c length)))

(define (ucs-2-len/c length)
  (and/c ucs-2? (string-len/c length)))
