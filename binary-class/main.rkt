#lang racket/base
(require "base.rkt" "common.rkt" "string.rkt")
(provide (all-from-out "base.rkt" "common.rkt" "string.rkt"))

(module unsafe racket/base
  (require (submod "base.rkt" unsafe) (submod "common.rkt" unsafe) (submod "string.rkt" unsafe))
  (provide (all-from-out (submod "base.rkt" unsafe)
                         (submod "common.rkt" unsafe)
                         (submod "string.rkt" unsafe))))

(module safe racket/base
  (require (submod "base.rkt" safe) (submod "common.rkt" safe) (submod "string.rkt" safe))
  (provide (all-from-out (submod "base.rkt" safe)
                         (submod "common.rkt" safe)
                         (submod "string.rkt" safe))))