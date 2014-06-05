#lang racket/base
(require racket/contract (submod "base.rkt" unsafe))

(module unsafe racket/base
  (require (submod "base.rkt" unsafe))
  (provide unsigned-integer unsigned-integer-le u1 u2 u3 u4 l1 l2 l3 l4 discard bytestring)
  
  (define (unsigned-integer bytes [bits-per-byte 8])
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
       (let loop ([shift (+ max-shift bits-per-byte)] [byte 1])
         (define next-shift (- shift bits-per-byte))
         (write-byte (bitwise-bit-field value next-shift shift) out)
         (if (= byte bytes)
             (void)
             (loop next-shift (add1 byte)))))))
  
  (define u1 (unsigned-integer 1))
  (define u2 (unsigned-integer 2))
  (define u3 (unsigned-integer 3))
  (define u4 (unsigned-integer 4))
  
  (define (unsigned-integer-le bytes [bits-per-byte 8])
    (binary
     (λ (in)
       (let loop ([value 0] [shift 0] [byte 0])
         (if (= byte bytes) 
             value
             (loop (+ value (arithmetic-shift (read-byte in) shift))
                   (+ shift bits-per-byte)
                   (add1 byte)))))
     (λ (out value)
       (let loop ([shift 0] [byte 1])
         (define next-shift (+ shift bits-per-byte))
         (write-byte (bitwise-bit-field value shift next-shift) out)
         (if (= byte bytes)
             (void)
             (loop next-shift (add1 byte)))))))
  
  (define l1 (unsigned-integer-le 1))
  (define l2 (unsigned-integer-le 2))
  (define l3 (unsigned-integer-le 3))
  (define l4 (unsigned-integer-le 4))
  
  (define (discard length)
    (binary
     (λ (in)
       (read-bytes length)
       #f)
     (λ (out dummy)
       (write-bytes (make-bytes length) out)
       (void))))
  
  (define (bytestring length)
    (binary
     (λ (in)
       (read-bytes length))
     (λ (out value)
       (write-bytes value out 0 length)))))
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
  (define (binary/c value-contract) 
    (struct/c binary 
              (-> input-port? value-contract)
              (-> output-port? value-contract void?)))
  (define (in-bits/c bits) (between/c 0 (sub1 (arithmetic-shift 1 bits))))
  (define binary-integer/c
    (->i ([bytes exact-positive-integer?])
                          ([bits-per-byte exact-positive-integer?])
                          [result (bytes bits-per-byte)                                  
                                  (binary/c (in-bits/c (* bytes bits-per-byte)))]))
  (provide/contract
   [unsigned-integer binary-integer/c]
   [unsigned-integer-le binary-integer/c]
   [u1 (binary/c (in-bits/c 8))]
   [l1 (binary/c (in-bits/c 8))]
   [u2 (binary/c (in-bits/c 16))]
   [l2 (binary/c (in-bits/c 16))]
   [u3 (binary/c (in-bits/c 24))]
   [l3 (binary/c (in-bits/c 24))]
   [u4 (binary/c (in-bits/c 32))]
   [l4 (binary/c (in-bits/c 32))]
   [discard (binary/c #f)]
   [bytestring (binary/c bytes?)]))

(module+ test
  (require rackunit)
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