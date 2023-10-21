(module infer (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "types.scm")

  (provide bound-variables
           well-typed-expression?
           well-typed?)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Task 1
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; bound-variables : Program -> Listof[Symbol]
  ;;
  ;; (bound-variables pgm) returns a list of the variables that
  ;; occur bound in pgm or are bound in the standard initial
  ;; environment.

  ;; FIXME: your code goes here

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Renaming of bound variables is kind of awkward,
  ;; so the instructors provide this part of the code.
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; rename-bound-variables : Program -> Program
  ;;
  ;; (rename-bound-variables pgm) returns a program equivalent to pgm
  ;; in which all bound variables are unique.
  ;;
  ;; pre-condition: all of the procedure names must be distinct.

  (define (rename-bound-variables pgm)
    (set! renaming-counter 0)
    (cases program pgm
     (a-program (defns exp)
      (let* ((procnames (map (lambda (defn)
                               (cases definition defn
                                (proc-definition (id bvars body)
                                 id)))
                             defns))
             (env (extend-env* '(true false x) '(true false x) (empty-env)))
             (env (extend-env* procnames
                               (map fresh-variable procnames)
                               env)))
        (a-program (map (lambda (defn)
                          (rename-bound-variables-in-definition defn env))
                        defns)
                   (rename-bound-variables-in-expression exp env))))))

  ;; rename-bound-variables-in-definition : Definition * Env -> Definition
  ;;
  ;; (rename-bound-variables-in-definition defn env)
  ;;     returns a definition equivalent to defn in which
  ;;     all variables have been renamed

  (define (rename-bound-variables-in-definition defn env)
    (cases definition defn
     (proc-definition (id bvars body)
      (let* ((newid (apply-env env id))
             (newvars (map fresh-variable bvars))
             (env (extend-env* bvars newvars env)))
        (proc-definition newid
                         newvars
                         (rename-bound-variables-in-expression body env))))))

  ;; rename-bound-variables-in-expression : Exp * Env -> Exp
  ;;
  ;; (rename-bound-variables-in-expression exp env)
  ;;     renames all bound variables in exp according to env

  (define (rename-bound-variables-in-expression exp env)
    (cases expression exp

     (lit-exp (num) exp)

     (var-exp (id)
      (let ((newid (apply-env env id)))
        (if newid
            (var-exp newid)
            exp)))

     (binop-exp (op exp1 exp2)
      (binop-exp op
                 (rename-bound-variables-in-expression exp1 env)
                 (rename-bound-variables-in-expression exp2 env)))

     (if-exp (exp1 exp2 exp3)
      (if-exp (rename-bound-variables-in-expression exp1 env)
              (rename-bound-variables-in-expression exp2 env)
              (rename-bound-variables-in-expression exp3 env)))

     (call-exp (rator rands)
      (call-exp (rename-bound-variables-in-expression rator env)
                (map (lambda (exp)
                       (rename-bound-variables-in-expression exp env))
                     rands)))))

  ;; fresh-variable : Symbol -> Symbol
  ;;
  ;; (fresh-variable x) returns a newly generated symbol

  (define (fresh-variable x)
    (set! renaming-counter (+ renaming-counter 1))
    (string->symbol
     (string-append (symbol->string x)
                    "_"
                    (number->string renaming-counter))))

  (define renaming-counter 0)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Task 2
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; well-typed-expression? : Expression -> Boolean
  ;;
  ;; (well-typed-expression? exp) returns true if and only if
  ;; (a-program '() exp) is well-typed.

  ;; FIXME: your code goes here

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Task 3
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; well-typed? : Program -> Boolean
  ;;
  ;; (well-typed? pgm)
  ;; returns #t if pgm is well-typed, and
  ;; returns #f otherwise.

  (define (well-typed? pgm)
    (let* ((pgm (rename-bound-variables pgm))
           (tcs (collect-constraints pgm))
           (subst (solve-type-constraints tcs)))
      (if subst
          #t
          #f)))

  ;; collect-constraints : Program -> Listof[TypeConstraint]
  ;;
  ;; (collect-constraints pgm) returns a list of type constraints
  ;; that are satisfiable if and only pgm is well-typed.
  ;;
  ;; precondition: No variable is bound in more than one place.

  ;; FIXME: your code goes here

  )
