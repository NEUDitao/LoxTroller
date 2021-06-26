#lang racket

(require racket/lazy-require)
(require "expr.rkt")
(require "token.rkt")
(require "token-type.rkt")
(require "scanner.rkt")
(lazy-require ["lox.rkt" (lox-error)])
(provide parse)

(module+ test
  (require rackunit))

#;(: parse : [Listof token] -> (U Expr Null))
;; parsers a list of tokens, use (scanner-scan tokens) to get tokens from string
(define (parse tokens)
  (with-handlers ([exn:fail? (λ (e) (displayln "parse error...") null)])
    (let-values ([(expr curr) (expression tokens 0)])
      expr)))

#;(: bin-rule : [Listof token] Integer [Listof TokenType]
   ([Listof token] Integer -> (Values Expr Integer))
   -> (Values Expr Integer))
;; generic function to match on current bin-rule, and descend
(define (bin-rule tokens curr match-toks next-recurrence)
  (let-values ([(expr curr) (next-recurrence tokens curr)])
    (cond
      [(parse-match match-toks tokens curr)
       (define op (peek tokens curr))
       (let-values ([(right curr) (next-recurrence tokens (add1 curr))])
         (values (expr-binary expr op right) curr))]
      [else (values expr curr)])))

#;(: parse-match : [Listof TokenType] [Listof token] Integer -> Boolean)
;; determines if current token matches any of the wanted types
(define (parse-match types tokens curr)
  (ormap (λ (type) (check tokens curr type)) types))

#;(: check : [Listof token] Integer TokenType -> Boolean)
;; checks if current token has type
(define (check tokens curr type)
  (and (not (end? tokens curr))
       (eq? (token-type (peek tokens curr)) type)))

#;(: end? : [Listof token] Integer -> Boolean)
;; if at end
(define (end? tokens curr)
  (eq? 'EOF (token-type (peek tokens curr))))

#;(: previous : [Listof token] Integer -> token)
;; previous token
(define (previous tokens curr)
  (list-ref tokens (sub1 curr)))

#;(: peek : [Listof token] Integer -> token)
;; current token
(define (peek tokens curr)
  (list-ref tokens curr))

#;(: expression : [Listof token] Integer -> (Values Expr Integer))
;; highest parse rule
(define (expression tokens curr)
  (equality tokens curr))

#;(: equality : [Listof token] Integer -> (Values Expr Integer))
;; == and != rule
(define (equality tokens curr)
  (bin-rule tokens curr '(BANG_EQUAL EQUAL_EQUAL) comparison))

#;(: comparison : [Listof token] Integer -> (Values Expr Integer))
;; > >= < <= rule
(define (comparison tokens curr)
  (bin-rule tokens curr '(GREATER GREATER_EQUAL LESS LESS_EQUAL) term))

#;(: term : [Listof token] Integer -> (Values Expr Integer))
;; - and + rule (in a binary context)
(define (term tokens curr)
  (bin-rule tokens curr '(MINUS PLUS) factor))

#;(: factor : [Listof token] Integer -> (Values Expr Integer))
;; / and * rule
(define (factor tokens curr)
  (bin-rule tokens curr '(SLASH STAR) unary))

#;(: unary : [Listof token] Integer -> (Values Expr Integer))
;; ! and - rule (in a unary context)
(define (unary tokens curr)
  (cond
    [(parse-match '(BANG MINUS) tokens curr)
     (define op (peek tokens curr))
     (let-values ([(expr curr) (unary tokens curr)])
       (values (expr-unary op expr) curr))]
    [else (primary tokens curr)]))

#;(: primary : [Listof token] Integer -> (Values Expr Integer))
;; raw toks (false, true, nil, and `(' )
(define (primary tokens curr)
  (match (token-type (peek tokens curr))
    ;; ['EOF (values (expr-literal null) (add1 curr))]
    ['FALSE (values (expr-literal false) (add1 curr))]
    ['TRUE (values (expr-literal true) (add1 curr))]
    ['NIL (values (expr-literal null) (add1 curr))]
    [(or 'NUMBER 'STRING)
     (values (expr-literal (peek tokens curr)) (add1 curr))]
    ['LEFT_PAREN
     (let-values ([(expr curr) (expression tokens (add1 curr))])
       (consume tokens curr 'RIGHT_PAREN "Expect ')' after expression.")
       (values (expr-grouping expr) (add1 curr)))]
    [else (parser-error (peek tokens curr) "Expect expression.")]))

#;(: consume : [Listof token] Integer TokenType String -> Integer)
;; parses until we reach that wanted tokentype
(define (consume tokens curr type message)
  (if (check tokens curr type)
      (add1 curr)
      (parser-error (peek tokens curr) message)))

#;(: parser-error : token String -> Void)
;; parse error.. is the reason we're not typed racket
(define (parser-error token message)
    (error (lox-error token message)))

#;(: synchronize : [Listof token] Integer -> Integer)
;; Gets us out of the panic state in the parser
(define (synchronize tokens curr)
  (cond
    [(end? tokens curr) curr]
    [(eq? (previous tokens curr) 'SEMICOLON) curr]
    [(member (peek tokens curr) '(CLASS FUN VAR FOR IF WHILE PRINT RETURN)) curr]
    [else (synchronize tokens (add1 curr))]))

(module+ test
  #;(: parser-tester : String Integer Expr (U False Integer) -> Any)
  (define (parser-tester program curr expected-expr expected-curr)
    (let-values ([(expr curr) (expression (scanner-scan program) curr)])
      (check-equal? expr expected-expr)
      (check-equal? curr expected-curr)))

  (parser-tester "true" 0 (expr-literal true) 1)
  (parser-tester "true == true" 0 (expr-binary (expr-literal #t)
                                               (token 'EQUAL_EQUAL "==" null 1)
                                               (expr-literal #t)) 3)
  (check-equal? (parse (scanner-scan "==")) null)
  )

