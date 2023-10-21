#lang eopl

;; grammar for the PROC language

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
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression
     ("greater" "(" expression "," expression ")")
     greater-exp)
    
    (expression
     ("less" "(" expression  "," expression ")")
     less-exp)
    
    (expression
     ("equal" "(" expression  "," expression ")")
     equal-exp)
    
    (expression (identifier) var-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    ;; Write the grammar for my-cond for Problem 1

    (expression
     ("my-cond" expression "then" expression "," (arbno expression "then" expression ",") "else" expression)
     my-cond-exp)

    ; Modify grammer of proc and call exp for Problem 2
    ; Hint: We want arguments to be seperated by ','. You can use the function separated-list
    (expression
     ("proc" "(" (separated-list identifier ",") ")" expression)
     proc-exp)
    
    (expression
     ("(" expression (arbno expression) ")")
     call-exp)
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

