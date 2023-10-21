(module interp (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; the-top-level-environment : environment

  (define the-top-level-environment (init-env))

  ;; value-of-program : program -> expval

  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (defns exp)
          (set! the-top-level-environment
                (extend-env-with-defns defns (init-env)))
          (value-of exp
                    the-top-level-environment)))))

  ;; extend-env-with-defns : definitions * environment -> environment

  (define extend-env-with-defns
    (lambda (defns env)
      (cond ((null? defns)
             env)
            (else
             (cases definition (car defns)
               (proc-definition (id bvars body)
                 (extend-env-with-defns
                  (cdr defns)
                  (extend-env id
                              (proc-val (procedure bvars body))
                              env))))))))

  ;; value-of : expression * environment -> expval

  (define value-of
    (lambda (exp env)
      (cases expression exp

        (lit-exp (num) (num-val num))

        (var-exp (id)
          (let ((val (apply-env env id)))
            (if (expval? val)
                val
                (eopl:error 'value-of "Unbound variable"))))

        (binop-exp (op exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (apply-binop op val1 val2)))

        (if-exp (exp1 exp2 exp3) 
          (if (expval->bool (value-of exp1 env))
              (value-of exp2 env)
              (value-of exp3 env)))

        (call-exp (rator rands)
          (let ((proc (expval->proc (value-of rator env)))
                (args (map (lambda (rand) (value-of rand env))
                           rands)))
	    (apply-procedure proc args)))

        )))

  ;; apply-binop : binop * expval * expval -> expval

  (define apply-binop
    (lambda (op val1 val2)
      (cases binop op
        (op-plus    () (num-val (+ (expval->num val1) (expval->num val2))))
        (op-minus   () (num-val (- (expval->num val1) (expval->num val2))))
        (op-times   () (num-val (* (expval->num val1) (expval->num val2))))
        (op-less    () (bool-val (< (expval->num val1) (expval->num val2))))
        (op-equal   () (bool-val (= (expval->num val1) (expval->num val2))))
        (op-greater () (bool-val (> (expval->num val1) (expval->num val2)))))))

  ;; apply-procedure : procedure * expval -> expval

  (define apply-procedure
    (lambda (proc1 args)
      (cases proc proc1
        (procedure (bvars body)
          (value-of body
                    (extend-env* bvars args the-top-level-environment))))))
  
  )
