#lang racket

(require racket/lazy-require)
(provide lox-error)
(require "token.rkt")
(lazy-require ["scanner.rkt" (scanner-scan)])
(lazy-require ["parser.rkt" (parse)])

(define args (current-command-line-arguments))
(define args-length (vector-length args))
(define did-error #f)

#;(: run-file : String -> Void)
;; runs lox program at path
(define (run-file path)
  (define file-as-string (file->string path))
  (define error? (run file-as-string))
  (when error? (exit 65)))

#;(: run-prompt : -> Void)
;; REPL for lox
(define (run-prompt)
  (display "> ")
  (define user-input (read-line))
  (if
   (or (eof-object? user-input) (string=? user-input "exit"))
   (void)
   (begin
     (void (run user-input))
     (run-prompt))))

#;(: run : String -> Boolean)
;; scans a lox program and runs it. returns whether or not program errored
(define (run source)
  (define tokens (scanner-scan source))
  (define parsed (parse tokens))
  (when did-error #f)
  (displayln parsed)
  #t)

#;(: lox-error : (U Integer token) String -> Void)
(define (lox-error var message)
  (set! did-error #t)
  (match var
    [(? integer?) (report var "" message)]
    [(? token?)
     (report (token-line var)
                        (if (eq? 'EOF (token-type var))
                            " at end"
                            (format " at '~a'" (token-lexeme var)))
                        message)]))

;; Need some version of hadError so the code doesn't get executed

#;(: report : Integer String String -> Void)
(define (report line where message)
  (fprintf (current-error-port) "[line ~a] Error~a: ~a" line where message))

(define (main)
  (cond
    [(> args-length 1) (displayln "Usage: racklox [script]")]
    [(= args-length 1) (run-file (vector-ref args 0))]
    [else (displayln "Welcome to the Lox Interpreter") (run-prompt)]))

(module+ main (main))
