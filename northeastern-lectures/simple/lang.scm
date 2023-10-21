;;; a simple language

(module lang
  (lib "eopl.ss" "eopl")                
  
  (require "drscheme-init.scm")
  
  (provide (all-defined))

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
    '((program ((arbno definition) expression) a-program)

      (definition
        ("define" identifier "=" "proc" "(" (arbno identifier) ")" expression)
        proc-definition)

      (expression (number) lit-exp)

      (expression (identifier) var-exp)

      (expression
        (binop "(" expression "," expression ")")
        binop-exp)

      (expression
        ("if" expression "then" expression "else" expression)
        if-exp)

      (expression
        ("(" expression (arbno expression) ")")
        call-exp)

      (binop ("+") op-plus)
      (binop ("-") op-minus)
      (binop ("*") op-times)

      (binop ("<") op-less)
      (binop ("=") op-equal)
      (binop (">") op-greater)
      
      ))

  (define the-field-names
    '((a-program defns exp)
      (proc-definition id bvars body)
      (lit-exp n)
      (var-exp id)
      (binop-exp op e1 e2)
      (if-exp e1 e2 e3)
      (call-exp rator rands)
      ))
  
  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
