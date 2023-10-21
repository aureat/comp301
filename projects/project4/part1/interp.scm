(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LETREC language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")
  (require "globalstack.scm")
  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-translation value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

(define value-of-translation
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
                   (value-of exp1 (init-env))))))

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))
  
  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 83
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (difference-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (lett-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of letrec-body
            (extend-env-rec p-name b-var p-body env)))
        
        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### You need to implement the value-of methods of
        ; ###### proc-nested-exp, call-nested-exp, and 
        ; ###### letrec-nested-exp. They should be similar to 
        ; ###### the proc-exp, letrec-exp and call-exp versions.
        ; ###### However, please keep in mind the nested-procedure
        ; ###### type and extend-env-rec-nested procedure in other
        ; ###### files while implementing this structure. 
        ; #####################################################

        (proc-nested-exp (var count-var name body)
          (let ((count (expval->num (value-of (var-exp count-var) env))))
            (proc-val (nested-procedure name var body env count))))
                         
        (call-nested-exp (rator rand count-exp)
           (let ((arg (value-of rand env))
                 (updated-count (expval->num (value-of count-exp env)))
                 (procedure (expval->proc (value-of rator env))))
             (apply-procedure
              (cases proc procedure
                (nested-procedure (name var body saved-env count)
                  (nested-procedure name var body saved-env updated-count))
                (else procedure))
              arg)))

        (letrec-nested-exp (p-name b-var count-var p-body letrec-body)
           (let ((count (expval->num (value-of (var-exp count-var) env))))
            (value-of letrec-body
               (extend-env-rec-nested p-name b-var p-body env count))))

        ; #####################################################
      
      )))


  ;; apply-procedure : Proc * ExpVal -> ExpVal

  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of body (extend-env var arg saved-env)))
        
        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### Here, you need to handle the case where proc 
        ; ###### is not a procedure but a nested-procedure. 
        ; ###### Since we also keep the count as a parameter, 
        ; ###### we also need to extend the environment with
        ; ###### the count value.
        ; ###### Additionally, please note that no matter how the
        ; ###### procedure is defined, all procedures are applied
        ; ###### by using this part. This information may be
        ; ###### helpful for your implementation. 
        ; ###### You will also do the prints here. You can use
        ; ###### "recursive-displayer" function defined below.
        ; #####################################################

        (nested-procedure (name var body saved-env count)
          (begin
            (recursive-displayer name count)
            (value-of body (extend-env 'count (num-val count)
                                       (extend-env var arg saved-env)))
          ))

        ; #####################################################
      
      )))
  
    (define recursive-displayer
      (lambda (name num)
        (if (zero? num) (begin
          (display name)
          (display " --> ")
          (display num)
          (newline)
        )
        (begin
          (recursive-displayer-helper name (- num 1))
          (display num)
          (newline))
    )))

    (define recursive-displayer-helper
      (lambda (name num)
        (if (zero? num) 
          (begin
            (display name)
            (display " --> ")
          )

        (begin
          (display "....")
          (recursive-displayer-helper name (- num 1))
        )
    ))
    )
  )
  


  
