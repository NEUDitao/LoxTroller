#lang typed/racket

(require "token-type.rkt")

(provide token token-lexeme token-type token-literal token-line token?)

(struct token
  ([type : TokenType] [lexeme : String] [literal : Any] [line : Integer])
  #:transparent)

(: print-token : token -> String)
(define (print-token token)
  (format "~a ~a ~a"
           (token-type token) (token-lexeme token) (token-literal token)))
