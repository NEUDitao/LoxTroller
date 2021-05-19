#lang typed/racket

(provide TokenType KEYWORDS)

(define-type TokenType (U
                    ;; Single Character Tokens
                    'LEFT_PAREN 'RIGHT_PAREN 'LEFT_BRACE 'RIGHT_BRACE 'COMMA
                    'DOT 'MINUS 'PLUS 'SEMICOLON 'SLASH 'STAR

                    ;; One/Two Char tokens
                    'BANG 'BANG_EQUAL 'EQUAL 'EQUAL_EQUAL 'GREATER
                    'GREATER_EQUAL 'LESS 'LESS_EQUAL

                    ;; Literals
                    'IDENTIFIER 'STRING 'NUMBER

                    ;; Keywords
                    'AND 'CLASS 'ELSE 'FALSE 'FUN 'FOR 'IF 'NIL 'OR 'PRINT
                    'RETURN 'SUPER 'THIS 'TRUE 'VAR 'WHILE

                    'EOF))

(define KEYWORDS : (Listof (List String TokenType))
  '(("and" AND)
    ("class" CLASS)
    ("else" ELSE)
    ("false" FALSE)
    ("fun" FUN)
    ("for" FOR)
    ("if" IF)
    ("nil" NIL)
    ("or" OR)
    ("print" PRINT)
    ("return" RETURN)
    ("super" SUPER)
    ("this" THIS)
    ("true" TRUE)
    ("var" VAR)
    ("while" WHILE)))
