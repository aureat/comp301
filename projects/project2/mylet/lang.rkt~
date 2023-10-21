#lang eopl

;; grammar for the LET language  

(provide (all-defined-out))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
  ))


(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)

    (expression ("zero?" "(" expression ")") zero?-exp)
    
    (expression (identifier) var-exp)
    
    (expression ("let" identifier "=" expression "in" expression) let-exp)

    (expression
     ("if" expression "then" expression (arbno "elif" expression "then" expression) "else" expression)
     if-exp)

    (expression ("op" "(" expression "," expression "," number ")") op-exp)

    (expression ("sum" "(" expression ")") sum-exp)
    
    (expression ("(" number "/" number ")") rational-exp)

    (expression ("simpl" "(" expression ")") simpl-exp)

    (expression ("create-new-list" "(" ")") new-list-exp)

    (expression ("cons" expression "to" expression) cons-exp)
    
))


;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

