#lang typed/racket

(provide lox-error)

(define args (current-command-line-arguments))
(define args-length (vector-length args))

(: run-file : String -> Void)
;; runs lox program at path
(define (run-file path)
  (define file-as-string (file->string path))
  (define error? (run file-as-string))
  (when error? (exit 65)))

(: run-prompt : -> Void)
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

(: run : String -> Boolean)
;; scans a lox program and runs it. returns whether or not program errored
(define (run source)
  #;(scanner goes here)
  #t)

(: lox-error : Integer String -> Void)
(define (lox-error line message)
  (report line "" message))

;; Need some version of hadError so the code doesn't get executed

(: report : Integer String String -> Void)
(define (report line where message)
  (fprintf (current-error-port) "[line ~a] Error~a: ~a" line where message))

(define (main)
  (cond
    [(> args-length 1) (displayln "Usage: racklox [script]")]
    [(= args-length 1) (run-file (vector-ref args 0))]
    [else (displayln "Welcome to the Lox Interpreter") (run-prompt)]))

(module+ main (main))
