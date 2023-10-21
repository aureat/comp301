(module environments (lib "eopl.ss" "eopl") 
  
  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm. 

  (require "data-structures.scm")

  (provide init-env empty-env extend-env apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 6

  (define init-env 
    (lambda ()
      
      ; #####################################################
      ; ###### ENTER YOUR CODE HERE
      ; ###### you need to extend the environment with count 
      ; ###### variable so that it could be manipulated and
      ; ###### increased later on.
      ; #####################################################    

      (extend-env
       'count (num-val 0)
    
      ; #####################################################
       
       (extend-env
        'i (num-val 1)
        (extend-env
        'v (num-val 5)
        (extend-env
         'x (num-val 10)
         (empty-env)))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  ;; Page: 86
  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search-sym))
        
        (extend-env (var val saved-env)
          (if (eqv? search-sym var)
              
              ; #####################################################
              ; ###### ENTER YOUR CODE HERE, YOU MAY DELETE
              ; ###### THE CODE BELOW, IT IS PUT TO PROVIDE A RUNNING
              ; ###### CODE BASELINE.
              ; ######
              ; ###### You need to check the given value, and take 
              ; ###### care of the case where the given value is a 
              ; ###### nested-procedure. If it is a nested-procedure, 
              ; ###### a proc-val with a nested-procedure should be 
              ; ###### returned. Otherwise, it should behave
              ; ###### as it normally does.
              ; #####################################################
              
              (cases expval val
                (proc-val (procval)
                  (cases proc procval
                    (nested-procedure (name bvar body env count)
                       (proc-val (nested-procedure var bvar body env count)))
                    (else procval)))
                (else val))                    

              ; #####################################################
                        
              (apply-env saved-env search-sym))
        )
        
        (extend-env-rec (p-name b-var p-body saved-env)
          (if (eqv? search-sym p-name)
            (proc-val (procedure b-var p-body env))          
            (apply-env saved-env search-sym)))
        
        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### You need to add the variant extend-env-rec-nested, 
        ; ###### for the nested procedures.
        ; #####################################################

        (extend-env-rec-nested (id bvar body saved-env count)
          (if (eqv? search-sym id)
              (proc-val (nested-procedure id bvar body env count))
              (apply-env saved-env search-sym)))

        ; #####################################################
      )
    )
  )
)