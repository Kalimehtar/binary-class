#lang racket/base
(module unsafe racket/base
  (require (submod "base.rkt" unsafe) "common.rkt")
  (provide generic-string
           generic-terminated-string
           iso-8859-1-string
           iso-8859-1-terminated-string
           ucs-2-string
           ucs-2-terminated-string)
  
  (define (generic-string length character-type)
    (binary
     (λ (in)
       (define string (make-string length))
       (for ([i (in-range length)])
         (string-set! string i (read-value character-type in)))
       string)
     (λ (out string)
       (for ([i (in-range length)])
         (write-value character-type out (string-ref string i)))
       (void))))
  
  (define (generic-terminated-string terminator character-type)
    (binary
     (λ (in)    
       (define string-port (open-output-string))
       (let loop ()
         (define char (read-value character-type in))
         (unless (char=? char terminator)
           (write-char char string-port)
           (loop)))
       (get-output-string string-port))
     (λ (out string)
       (for ([c (in-string string)])
         (write-value character-type out c))
       (write-value character-type out terminator)
       (void))))
  
  (define iso-8859-1-char
    (binary
     (λ (in)
       (integer->char (read-byte in)))
     (λ (out char)
       (define byte (char->integer char))
       (when (> byte 255)
         (raise-arguments-error 'iso-8859-1-char "code should be in [0, 255]" "char code" byte))
       (write-byte byte out)
       (void))))
  
  (define (iso-8859-1-string length) (generic-string length iso-8859-1-char))
  
  (define (iso-8859-1-terminated-string [terminator #\nul])
    (generic-terminated-string terminator iso-8859-1-char)) 
  
  (define (ucs-2-char swap)
    (define type (if swap l2 u2))
    (binary
     (λ (in)
       (integer->char (read-value type in)))
     (λ (out char)
       (define code (char->integer char))
       (when (> code 65535)
         (raise-arguments-error 'ucs-2-char "code should be in [0, 65535]" "char code" code))
       (write-value type out code)
       (void))))
  
  (define (ucs-2-char-type byte-order-mark)
    (ucs-2-char 
     (case byte-order-mark
       ((#xfeff) #f)
       ((#xfffe) #t))))
  
  (define (ucs-2-string length)
    (define characters (sub1 (/ length 2)))
    (binary 
     (λ (in)
       (define byte-order-mark (read-value u2 in))
       (read-value (generic-string characters (ucs-2-char (ucs-2-char-type byte-order-mark))) in))
     (λ (out value)
       (write-value u2 out #xfeff)
       (write-value (generic-string characters (ucs-2-char #f)) out value))))
  
  (define (ucs-2-terminated-string [terminator #\nul])
    (define characters (sub1 (/ length 2)))
    (binary 
     (λ (in)
       (define byte-order-mark (read-value u2 in))
       (read-value (generic-terminated-string terminator (ucs-2-char-type byte-order-mark)) in))
     (λ (out value)
       (write-value u2 out #xfeff)
       (write-value (generic-terminated-string terminator (ucs-2-char #f)) out value)))))

(require racket/contract 'unsafe (submod "base.rkt" unsafe))

(provide/contract 
 [generic-string (-> exact-positive-integer? binary? any)]
 [generic-terminated-string (-> char? binary? any)]
 [iso-8859-1-string (-> exact-positive-integer? any)]
 [iso-8859-1-terminated-string (->* () (char?) any)]
 [ucs-2-string (-> exact-positive-integer? any)]
 [ucs-2-terminated-string (->* () (char?) any)])

(module* safe #f
  (require "contract.rkt")
  (provide/contract 
   [generic-string (->i ([length exact-positive-integer?] [char (binary/c char?)])
                        [result (length) (binary/c (string-len/c length))])]
   [generic-terminated-string (->i ([terminator char?] [char (binary/c char?)])
                                   [result (terminator) 
                                           (binary/c (string-terminated/c terminator))])]
   [iso-8859-1-string (->i ([length exact-positive-integer?])
                           [result (length) (binary/c (iso-8859-1-len/c length))])]
   [iso-8859-1-terminated-string (->i () 
                                      ([terminator char?]) 
                                      [result (terminator) 
                                           (binary/c (string-terminated/c terminator 
                                                                          iso-8859-1-char?))])]
   [ucs-2-string (->i ([length exact-positive-integer?])
                      [result (length) (binary/c (ucs-2-len/c length))])]
   [ucs-2-terminated-string (->i () 
                                 ([terminator char?]) 
                                 [result (terminator) 
                                         (binary/c (string-terminated/c terminator 
                                                                        ucs-2-char?))])]))