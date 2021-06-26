#lang typed/racket

(provide Expr expr-binary expr-grouping expr-literal expr-unary)
(require "token.rkt")

(define-type Expr (U expr-binary expr-grouping expr-literal expr-unary))

(struct expr-binary
  ([left : Expr] [operator : token] [right : Expr]) #:transparent)

(struct expr-grouping
  ([expr : Expr]) #:transparent)

(struct expr-literal
  ([value : Any]) #:transparent)

(struct expr-unary
  ([operator : token] [right : Expr]) #:transparent)

(: ast-printer : Expr -> String)
(define (ast-printer expr)
  (match expr
    [(? expr-binary?) (format "(~a ~a ~a)"
                              (token-lexeme (expr-binary-operator expr))
                              (ast-printer (expr-binary-left expr))
                              (ast-printer (expr-binary-right expr)))]
    [(? expr-grouping?) (format "(group ~a)"
                                (ast-printer (expr-grouping-expr expr)))]
    [(? expr-literal?) (format "~a" (if (null? (expr-literal-value expr))
                                        "nil"
                                        (expr-literal-value expr)))]
    [(? expr-unary?) (format "(~a ~a)"
                             (token-lexeme (expr-unary-operator expr))
                             (ast-printer (expr-unary-right expr)))]))

(module+ test
  (require typed/rackunit)
  (check-equal? (ast-printer (expr-binary (expr-unary (token 'MINUS "-" null 1)
                                                      (expr-literal 123))
                             (token 'STAR "*" null 1)
                             (expr-grouping (expr-literal 45.67))))
                             "(* (- 123) (group 45.67))"))
