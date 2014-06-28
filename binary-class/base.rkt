#lang racket/base
(require racket/contract/base racket/class "syntax.rkt" "private/base.rkt")

(provide/contract
 [binary (-> (-> input-port? any)
             (or/c (->* (output-port? any/c) #:rest list? any)
                   (-> output-port? any/c any))
             binary?)]
 [binary? (-> any/c boolean?)]
 [write-value (->* (any/c output-port? any/c) #:rest list? void?)]
 [read-value (->* (any/c input-port?) #:rest list? any)]
 [binary<%> interface?])
(provide define-binary-class)

(module+ test
  (require rackunit)
  (displayln "Testing...")
  (define u1 (binary (λ (in) 'ok) (λ (value out) (void))))
  (define u2 (binary (λ (in) 'ok2) (λ (value out) (void))))
  (test-begin
   (define-binary-class test ((a u1) (_ u1) (_ u1)))
   (define-binary-class test2 test ((b u2)))
   (define tmp (read-value test2 1))
   (check-eq? (get-field a tmp) 'ok)
   (check-eq? (get-field b tmp) 'ok2))
  (test-begin
   (define-binary-class disp ((a read-val)) #:dispatch (if (= a 1) disp2 disp3))
   (define read-val (binary (λ (in) in) (λ (value out) (void))))
   (define return-val (λ (x) (binary (λ (in) x) (λ (value out) (void)))))
   (define-binary-class disp2 disp ((b u1)))
   (define-binary-class disp3 disp ((b u2)))
   (check-eq? (get-field b (read-value disp 1)) 'ok)
   (check-eq? (get-field b (read-value disp 2)) 'ok2)
   (define-binary-class disp4 disp ((b (return-val (+ a 1)))))
   (check-eqv? (get-field b (read-value disp4 2)) 3)
   (check-eqv? (get-field b (read-value disp4 8)) 9)))

(module* safe #f
  (provide/contract
   ;; TODO: check that read returns the same number of values as write takes
   [binary (-> (-> input-port? any)
               (or/c (->* (output-port? any/c) #:rest list? void?)
                     (-> output-port? any/c void?))
               binary?)]
   [binary? (-> any/c boolean?)]
   [write-value (->* (any/c output-port? any/c) #:rest list? void?)]
   [read-value (->* (any/c input-port?) #:rest list? any)]
   [binary<%> interface?])
  (provide define-binary-class))
