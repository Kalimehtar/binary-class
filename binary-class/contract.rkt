#lang racket/base
(require racket/class racket/contract (submod "base.rkt" unsafe))
(provide binary-class/c 
         (contract-out [binary-integer/c (->* (exact-integer?) (exact-integer?) flat-contract?)]
                       [u1? flat-contract?]
                       [u2? flat-contract?]
                       [u3? flat-contract?]
                       [u4? flat-contract?]
                       [iso-8859-1-string? flat-contract?]
                       [ucs-2-string? flat-contract?]
                       [binary/c (-> contract? contract?)]
                       [iso-8859-1-char? flat-contract?]
                       [ucs-2-char? flat-contract?]
                       [string-terminated/c (->* (char?) (flat-contract?) flat-contract?)]
                       [iso-8859-1-len/c (-> real? flat-contract?)]
                       [ucs-2-len/c (-> real? flat-contract?)]
                       [iso-8859-1-terminated/c (-> char? flat-contract?)]
                       [ucs-2-terminated/c (-> char? flat-contract?)]))

(define-syntax-rule (binary-class/c CLASS BODY ...)
  (class/c
   BODY ...
   [read (->*m (input-port?) 
               (list? boolean? (or/c (is-a?/c binary<%>) #f))
               (is-a?/c CLASS))]
   [write (->m output-port? void?)]))

(define (binary/c value-contract) 
  (struct/c binary 
            (-> input-port? value-contract)
            (-> output-port? value-contract void?)))

(define (binary-integer/c bytes [bits-per-byte 8])
  (or/c #f (integer-in 0 (sub1 (expt 2 (* bytes bits-per-byte))))))

(define u1? (binary-integer/c 1))
(define u2? (binary-integer/c 2))
(define u3? (binary-integer/c 3))
(define u4? (binary-integer/c 4))

(define (stringof/c char-contract)
 (flat-named-contract
  `(stringof/c ,(contract-name char-contract))                     
   (λ (s)
     (for/and ([c (in-string s)]) 
       (char-contract c)))))

(define iso-8859-1-char? (flat-named-contract 'iso-8859-1-char (λ (c) (< (char->integer c) 256))))
(define ucs-2-char? (flat-named-contract 'ucs-2-char? (λ (c) (< (char->integer c) 65536))))

(define iso-8859-1-string?
  (or/c #f (and/c string? (stringof/c iso-8859-1-char?))))

(define ucs-2-string?
  (or/c #f (and/c string? (stringof/c ucs-2-char?))))

(define (iso-8859-1-len/c length)
  (and/c iso-8859-1-string? (string-len/c length)))

(define (ucs-2-len/c length)
  (and/c ucs-2-string? (string-len/c length)))

(define (string-terminated/c terminator [char-contract any/c])
  (or/c #f (and/c string? (stringof/c (and/c char-contract (not/c terminator))))))

(define (iso-8859-1-terminated/c terminator)
  (string-terminated/c terminator iso-8859-1-char?))

(define (ucs-2-terminated/c terminator)
  (string-terminated/c terminator ucs-2-char?))
