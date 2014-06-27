#lang racket/base
(require racket/contract (submod "base.rkt" unsafe))

(module unsafe racket/base
  (require (submod "base.rkt" unsafe))
  (provide unsigned-integer unsigned-integer-le u1 u2 u3 u4 l1 l2 l3 l4 discard bytestring
           signed integer-be integer-le
           float-be float-le double-be double-le current-position ref move-position constant)
  
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
  
  (define (real size be?)
    (binary
     (λ (in) 
       (floating-point-bytes->real (read-bytes size in) be?))
     (λ (out value) 
       (write-bytes (real->floating-point-bytes	value size be?) out)
       (void))))
  
  (define float-be (real 4 #t))
  (define float-le (real 4 #f))
  (define double-be (real 8 #t))
  (define double-le (real 8 #f))
  
  (define current-position
    (binary
     (λ (in)
       (file-position in))
     (λ (out dummy)       
       (void))))
  
  (define-syntax-rule (with-current-position STREAM BODY ...)
    (let ([save #f] [stream STREAM])
      (dynamic-wind 
       (λ () (set! save (file-position stream)))
       (λ () BODY ...)
       (λ () (file-position stream save)))))
  
  (define (ref type position)
    (binary
     (λ (in)
       (with-current-position in
          (file-position in position)
          (read-value type in)))
     (λ (out value)
       (with-current-position out
         (file-position out position)
         (write-value type out value)))))
  
  (define (move-position position)
    (binary
     (λ (in)
       (file-position in position)
       #f)
     (λ (out value)
       (file-position out position))))
  
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
       (write-bytes value* out 0 length)
       (void))))
  
  (define (constant bytes)
    (binary
     (λ (in)
       (define tmp (read-bytes (bytes-length bytes) in))
       (unless (equal? bytes tmp)
         (raise-arguments-error 'constant "invalid signature" "should be" bytes "got" tmp))
       #f)
     (λ (out value)
       (write-bytes bytes out)
       (void)))))
(require 'unsafe)

(provide/contract
 [integer-be (->* (exact-positive-integer?) (exact-positive-integer?) binary?)]
 [integer-le (->* (exact-positive-integer?) (exact-positive-integer?) binary?)]
 [signed (->* ((-> exact-positive-integer? exact-positive-integer? binary?) exact-positive-integer?)
              (exact-positive-integer?) binary?)]
 [u1 binary?]
 [u2 binary?]
 [u3 binary?]
 [u4 binary?]
 [l1 binary?]
 [l2 binary?]
 [l3 binary?]
 [l4 binary?]
 [float-be binary?]
 [float-le binary?]
 [double-be binary?]
 [double-le binary?]
 [discard (-> exact-positive-integer? binary?)]
 [bytestring (-> exact-positive-integer? binary?)]
 [current-position binary?] 
 [ref (-> binary? exact-nonnegative-integer? binary?)]
 [move-position (-> exact-nonnegative-integer? binary?)]
 [constant (-> bytes? binary?)]
 ;; deprecated
 [unsigned-integer (->* (exact-positive-integer?) (exact-positive-integer?) binary?)]
 [unsigned-integer-le (->* (exact-positive-integer?) (exact-positive-integer?) binary?)])


(module* safe #f
  (require "contract.rkt")
  (define binary-unsigned-integer/c
    (->i ([bytes exact-positive-integer?])
         ([bits-per-byte exact-positive-integer?])
         [result (bytes bits-per-byte) (binary/c (unsigned-integer/c bytes bits-per-byte))]))
  (define binary-signed-integer/c
    (->i ([base-type binary-unsigned-integer/c]
          [bytes exact-positive-integer?])
         ([bits-per-byte exact-positive-integer?])
         [result (bytes bits-per-byte) (binary/c (signed-integer/c bytes bits-per-byte))]))
  (provide/contract
   [integer-be binary-unsigned-integer/c]
   [integer-le binary-unsigned-integer/c]
   [signed binary-signed-integer/c]
   [u1 (binary/c u1?)]
   [l1 (binary/c u1?)]
   [u2 (binary/c u2?)]
   [l2 (binary/c u2?)]
   [u3 (binary/c u3?)]
   [l3 (binary/c u3?)]
   [u4 (binary/c u4?)]
   [l4 (binary/c u4?)]
   [float-be (binary/c (or/c #f single-flonum?))]
   [float-le (binary/c (or/c #f single-flonum?))]
   [double-be (binary/c (or/c #f double-flonum?))]
   [double-le (binary/c (or/c #f double-flonum?))]
   [current-position (binary/c (or/c #f exact-nonnegative-integer?))]
   [ref (-> binary? exact-nonnegative-integer? binary?)]
   [move-position (-> exact-nonnegative-integer? (binary/c #f))]
   [discard (-> exact-positive-integer? (binary/c #f))]
   [bytestring (-> exact-positive-integer? (binary/c (or/c #f bytes?)))]
   [constant (-> bytes? (binary/c #f))]))

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
                             (get-output-bytes res)))) 12334435)
  (check-eqv? (round (* 1e3 (read-value float-be 
                                        (open-input-bytes (bytes #x41 #x89 #x33 #x33)))))
              17150.0))
