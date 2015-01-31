#lang typed/racket/base

(provide 1String
         1string?
         )

(require (for-syntax racket/base
                     syntax/parse
                     racket/list
                     ))

(define one? (make-predicate One))

(: string-with-length-1? : [Any -> Boolean : #:+ String])
(define (string-with-length-1? x)
  (if (string? x)
      (one? (string-length x))
      #f))
(define-type String-With-Length-1 (Opaque string-with-length-1?))

(define-syntax define-1String-type
  (lambda (stx)
    (syntax-parse stx
      [(def 1String)
       #:with (str ...) (map string (map integer->char (range 0 256)))
       #'(define-type 1String (U String-With-Length-1 str ...))])))
(define-1String-type 1String)

(define 1string? (make-predicate 1String))

