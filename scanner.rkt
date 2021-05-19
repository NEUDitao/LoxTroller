#lang typed/racket

(require "token-type.rkt")
(require "token.rkt")
(require "lox.rkt")

(: end? : String Integer -> Boolean)
(define (end? source current) (>= current (string-length source)))

(: scanner-scan-tokens : String Integer Integer -> [Listof token])
;; scans source starting from current/line
(define (scanner-scan-tokens source current line)
  (cond
    [(end? source current) (list (token 'EOF "" null line))]
    [else
     (define-values (tok curr next-line)
       (scanner-scan-token source current line))
     (if (not (null? tok))
         (cons tok
               (scanner-scan-tokens source curr next-line))
         (scanner-scan-tokens source curr next-line))]))

(: scanner-scan-token : String Integer Integer ->
   (Values (U token Null) Integer Integer))
;; scans a single token and returns both the token and the current location of
;; the scanner. TokenType is null if we should skip to the next lexable token
(define (scanner-scan-token source current line)
  (define start current)
  (define curr-char (string-ref source current))

  (: get-literal : Integer -> String)
  (define (get-literal current) (substring source start (add1 current)))

  (: =-based-lexemes : TokenType TokenType ->
     (Values token Integer Integer))
  ;; helper for lexemes that end in =
  (define (=-based-lexemes s1 s2)
    (if (char-match? #\= source (add1 current))
        (values (token s1 (get-literal (add1 current)) null line)
                (+ 2 current) line)
        (single-lexeme s2)))

  (: find-newline : Integer -> Integer)
  (define (find-newline current)
    (if (or (char=? (scanner-peek source current) #\newline)
            (end? source current))
        current
        (find-newline (add1 current))))

  (: single-lexeme : TokenType -> (Values token Integer Integer))
  (define (single-lexeme lexeme)
    (values (token lexeme (get-literal current) null line) (add1 current)
            line))
  (match curr-char
    [#\( (single-lexeme 'LEFT_PAREN)]
    [#\) (single-lexeme 'RIGHT_PAREN)]
    [#\{ (single-lexeme 'LEFT_BRACE)]
    [#\} (single-lexeme 'RIGHT_BRACE)]
    [#\, (single-lexeme 'COMMA)]
    [#\. (single-lexeme 'DOT)]
    [#\- (single-lexeme 'MINUS)]
    [#\+ (single-lexeme 'PLUS)]
    [#\; (single-lexeme 'SEMICOLON)]
    [#\* (single-lexeme 'STAR)]
    [#\! (=-based-lexemes 'BANG_EQUAL 'BANG)]
    [#\= (=-based-lexemes 'EQUAL_EQUAL 'EQUAL)]
    [#\< (=-based-lexemes 'LESS_EQUAL 'LESS)]
    [#\> (=-based-lexemes 'GREATER_EQUAL 'GREATER)]
    [#\/ (if (char-match? #\/ source (add1 current))
             (values null (find-newline current) line)
             (single-lexeme 'SLASH))]
    [#\newline (values null (add1 current) (add1 line))]
    [(or #\tab #\return #\space) (values null (add1 current) line)]
    [#\" (scanner-string source current current line)]
    [c
     (cond
       [(char-numeric? c) (scanner-number source current #f current line)]
       [(char-alphabetic? c) (scanner-identifier source current current line)]
       [else
        (void (lox-error line (format "Unexpected character ~a." c)))
        (values null (add1 current) line)])]))

(module+ test
  (require typed/rackunit)
  (check-equal? (scanner-scan-tokens "(" 0 1)
                (list (token 'LEFT_PAREN "(" null 1)
                      (token 'EOF "" null 1)))
  (check-equal? (scanner-scan-tokens "(),.-" 0 1)
                (list
                 (token 'LEFT_PAREN "(" '() 1)
                 (token 'RIGHT_PAREN ")" '() 1)
                 (token 'COMMA "," '() 1)
                 (token 'DOT "." '() 1)
                 (token 'MINUS "-" '() 1)
                 (token 'EOF "" '() 1)))
  (check-equal? (let* ([s (open-output-string)]
                       [error-output (parameterize ([current-error-port s])
                                       (scanner-scan-tokens "@" 0 1))])
                  (list error-output (get-output-string s)))
                (list (list (token 'EOF "" '() 1))
                      "[line 1] Error: Unexpected character @."))
  (check-equal? (scanner-scan-tokens "!!=" 0 1)
                (list (token 'BANG "!" '() 1)
                      (token 'BANG_EQUAL "!=" '() 1)
                      (token 'EOF "" '() 1)))
  (check-equal? (scanner-scan-tokens "====<>>=" 0 1)
                (list
                 (token 'EQUAL_EQUAL "==" '() 1)
                 (token 'EQUAL_EQUAL "==" '() 1)
                 (token 'LESS "<" '() 1)
                 (token 'GREATER ">" '() 1)
                 (token 'GREATER_EQUAL ">=" '() 1)
                 (token 'EOF "" '() 1))               )
  (check-equal? (scanner-scan-tokens "// this is a comment\n*" 0 1)
                (list (token 'STAR "*" '() 2)
                      (token 'EOF "" '() 2)))
  (check-equal? (scanner-scan-tokens "+\n*" 0 1)
                (list (token 'PLUS "+" '() 1)
                      (token 'STAR "*" '() 2)
                      (token 'EOF "" '() 2)))
  (check-equal? (scanner-scan-tokens "{/  \t};" 0 1)
                (list
                 (token 'LEFT_BRACE "{" '() 1)
                 (token 'SLASH "/" '() 1)
                 (token 'RIGHT_BRACE "}" '() 1)
                 (token 'SEMICOLON ";" '() 1)
                 (token 'EOF "" '() 1)))
  (check-equal? (scanner-scan-tokens "//" 0 1)
                (list (token 'EOF "" '() 1)))
  (check-equal? (scanner-scan-tokens "\"test\"" 0 1)
                (list (token 'STRING "test" "test" 1)
                      (token 'EOF "" '() 1)))
  (check-equal? (let* ([s (open-output-string)]
                       [error-output (parameterize ([current-error-port s])
                                       (scanner-scan-tokens "\"" 0 1))])
                  (list error-output (get-output-string s)))
                (list (list (token 'EOF "" '() 1))
                      "[line 1] Error: Unterminated string."))
  (check-equal? (scanner-scan-tokens "\"te\nst\"*" 0 1)
                (list
                 (token 'STRING "te\nst" "te\nst" 2)
                 (token 'STAR "*" '() 2)
                 (token 'EOF "" '() 2)))
  (check-equal? (scanner-scan-tokens "69" 0 1)
                (list
                 (token 'NUMBER "69" 69 1)
                 (token 'EOF "" '() 1)))
  (check-equal? (scanner-scan-tokens "4.20" 0 1)
                (list (token 'NUMBER "4.20" 4.2 1) (token 'EOF "" '() 1)))
  (check-equal? (scanner-scan-tokens "42." 0 1)
                (list (token 'NUMBER "42" 42 1)
                      (token 'DOT "." null 1)
                      (token 'EOF "" null 1)))
  (check-equal? (scanner-scan-tokens "orifice or" 0 1)
                (list (token 'IDENTIFIER "orifice" null 1)
                      (token 'OR "or" null 1)
                      (token 'EOF "" null 1)))
  (check-equal? (scanner-scan-tokens "and class else false fun for if
                        nil or print return super this true var while" 0 1)
                (list
                 (token 'AND "and" '() 1)
                 (token 'CLASS "class" '() 1)
                 (token 'ELSE "else" '() 1)
                 (token 'FALSE "false" '() 1)
                 (token 'FUN "fun" '() 1)
                 (token 'FOR "for" '() 1)
                 (token 'IF "if" '() 1)
                 (token 'NIL "nil" '() 2)
                 (token 'OR "or" '() 2)
                 (token 'PRINT "print" '() 2)
                 (token 'RETURN "return" '() 2)
                 (token 'SUPER "super" '() 2)
                 (token 'THIS "this" '() 2)
                 (token 'TRUE "true" '() 2)
                 (token 'VAR "var" '() 2)
                 (token 'WHILE "while" '() 2)
                 (token 'EOF "" '() 2))))

(: char-match? : Char String Integer -> Boolean)
;; determines if char at current in source is what's expected
(define (char-match? expected source current)
  (and (not (end? source current))
       (char=? expected (string-ref source current))))

(module+ test
  (check-true (char-match? #\= "fuck = true" 5))
  (check-false (char-match? #\= "fuck = true" 6)))

(: scanner-peek : String Integer -> Char)
;; returns char at loc
(define (scanner-peek source loc)
  (if (end? source loc) (integer->char 0) (string-ref source loc)))

(: scanner-string : String Integer Integer Integer
   -> (Values (U token Null) Integer Integer))
(define (scanner-string source start current line)
  (define next-char (scanner-peek source (add1 current)))
  (cond
    [(and (not (char=? next-char #\")) (not (end? source current)))
     (scanner-string source
                     start
                     (add1 current)
                     (if (char=? next-char #\newline) (add1 line) line))]
    [(end? source current) (void (lox-error line "Unterminated string."))
                           (values null (add1 current) line)]
    [else (define substr (substring source (add1 start) (add1 current)))
          (values (token 'STRING substr substr line) (+ 2 current) line)]))

(: scanner-number : String Integer Boolean Integer Integer ->
   (Values token Integer Integer))
(define (scanner-number source start dec? current line)
  (define next-char (scanner-peek source (add1 current)))
  (cond
    [(char-numeric? next-char)
     (scanner-number source start dec? (add1 current) line)]
    [(and (char=? #\. next-char)
          (char-numeric? (scanner-peek source (+ 2 current)))
          (not dec?))
     (scanner-number source start #t (add1 current) line)]
    [else (define substr (substring source start (add1 current)))
          (values (token 'NUMBER substr (string->number substr) line)
                  (add1 current) line)]))

(: scanner-identifier : String Integer Integer Integer ->
   (Values token Integer Integer))
(define (scanner-identifier source start current line)
  (define next-char (scanner-peek source (add1 current)))
  (if
   (or (char-numeric? next-char) (char-alphabetic? next-char))
   (scanner-identifier source start (add1 current) line)
   (let* ([substr (substring source start (add1 current))]
          [possible-keyword (assoc substr KEYWORDS)]
          [tok-type (if possible-keyword (second possible-keyword)
                        'IDENTIFIER)])
     (values
      (token tok-type substr null line)
      (add1 current) line))))
