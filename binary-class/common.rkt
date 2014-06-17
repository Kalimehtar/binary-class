#lang racket/base
(require racket/contract (submod "base.rkt" unsafe))

(module unsafe racket/base
  (require (submod "base.rkt" unsafe))
  (provide unsigned-integer unsigned-integer-le u1 u2 u3 u4 l1 l2 l3 l4 discard bytestring
           signed integer-be integer-le)
  
  (define (integer-be bytes [bits-per-byte 8])
    (define max-shift (* (sub1 bytes) bits-per-byte))
    (binary
     (λ (in)
       (let loop ([value 0] [shift max-shift] [byte 0])
         (if (= byte bytes) 
             value
             (loop (+ value (arithmetic-shift (read-byte in) shift))
                   (- shift bits-per-byte)
                   (add1 byte)))))
     (λ (out value)
       (define value* (or value 0))
       (let loop ([shift (+ max-shift bits-per-byte)] [byte 1])
         (define next-shift (- shift bits-per-byte))
         (write-byte (bitwise-bit-field value* next-shift shift) out)
         (if (= byte bytes)
             (void)
             (loop next-shift (add1 byte)))))))
  
  (define (signed base bytes [bits-per-byte 8])
    (define max (expt 2 (sub1 (* bytes bits-per-byte))))
    (define base-type (base bytes bits-per-byte))
    (binary
     (λ (in)
       (define value (read-value base-type in))
       (if (>= value max) (- value max max) value))
     (λ (out value)
       (define value* (if (negative? value) (+ max (- value)) value))
       (write-value base-type out value))))
  
  (define unsigned-integer integer-be)
  
  (define u1 (integer-be 1))
  (define u2 (integer-be 2))
  (define u3 (integer-be 3))
  (define u4 (integer-be 4))
  
  (define (integer-le bytes [bits-per-byte 8])
    (binary
     (λ (in)
       (let loop ([value 0] [shift 0] [byte 0])
         (if (= byte bytes) 
             value
             (loop (+ value (arithmetic-shift (read-byte in) shift))
                   (+ shift bits-per-byte)
                   (add1 byte)))))
     (λ (out value)
       (define value* (or value 0))
       (let loop ([shift 0] [byte 1])
         (define next-shift (+ shift bits-per-byte))
         (write-byte (bitwise-bit-field value shift next-shift) out)
         (if (= byte bytes)
             (void)
             (loop next-shift (add1 byte)))))))
  
  (define unsigned-integer-le integer-le)
  
  (define l1 (integer-le 1))
  (define l2 (integer-le 2))
  (define l3 (integer-le 3))
  (define l4 (integer-le 4))
  
  (define (discard length)
    (binary
     (λ (in)
       (read-bytes length in)
       #f)
     (λ (out dummy)
       (write-bytes (make-bytes length) out)
       (void))))
  
  (define (bytestring length)
    (binary
     (λ (in)
       (read-bytes length in))
     (λ (out value)
       (define value* (or value #""))
       (write-bytes value* out 0 length)))))
(require 'unsafe)

(provide/contract
 [unsigned-integer (->* (exact-positive-integer?) (exact-positive-integer?) binary?)]
 [unsigned-integer-le (->* (exact-positive-integer?) (exact-positive-integer?) binary?)]
 [u1 binary?]
 [u2 binary?]
 [u3 binary?]
 [u4 binary?]
 [l1 binary?]
 [l2 binary?]
 [l3 binary?]
 [l4 binary?]
 [discard (-> exact-positive-integer? binary?)]
 [bytestring (-> exact-positive-integer? binary?)])

(module* safe #f
  (require "contract.rkt")
  (define binary-integer/c
    (->i ([bytes exact-positive-integer?])
         ([bits-per-byte exact-positive-integer?])
         [result (bytes bits-per-byte) (binary/c (binary-integer/c bytes bits-per-byte))]))
  (provide/contract
   [unsigned-integer binary-integer/c]
   [unsigned-integer-le binary-integer/c]
   [u1 (binary/c u1?)]
   [l1 (binary/c u1?)]
   [u2 (binary/c u2?)]
   [l2 (binary/c u2?)]
   [u3 (binary/c u3?)]
   [l3 (binary/c u3?)]
   [u4 (binary/c u4?)]
   [l4 (binary/c u4?)]
   [discard (-> exact-positive-integer? (binary/c #f))]
   [bytestring (-> exact-positive-integer? (binary/c (or/c #f bytes?)))]))

(module+ test
  (require rackunit)
  (check-eqv? (read-value (signed integer-be 4)
                          (open-input-bytes 
                           (let ([res (open-output-bytes)])
                             (write-value (signed integer-be 4) res -10)
                             (get-output-bytes res)))) -10)
  
  (check-eqv? (read-value (signed integer-be 4)
                          (open-input-bytes (bytes #b11111111 #b11111111 #b11111111 #b11100010)))
              -30)
  
  (check-eqv? (read-value u4 
                          (open-input-bytes 
                           (let ([res (open-output-bytes)])
                             (write-value u4 res 12334435)
                             (get-output-bytes res)))) 12334435)
  (check-eqv? (read-value l4 
                          (open-input-bytes 
                           (let ([res (open-output-bytes)])
                             (write-value l4 res 12334435)
                             (get-output-bytes res)))) 12334435))
