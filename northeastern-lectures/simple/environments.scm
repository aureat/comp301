(module environments (lib "eopl.ss" "eopl") 
  
  (require "data-structures.scm")
  (provide empty-env extend-env apply-env extend-env* init-env)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Static environments.
  ;;
  ;; These are just like the dynamic environments we have been
  ;; using, except
  ;;
  ;; 1.  Variables can be bound to any Scheme value, not just
  ;;     to the Scheme values that represent the expressed values
  ;;     of the language we are studying.
  ;; 2.  If x is not bound in env, then (apply-env env x)
  ;;     returns false instead of blowing up.
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; empty-env : -> Env
  ;; extend-env : Symbol * Any * Env -> Env
  ;; apply-env : Env * Symbol -> Any

  (define (empty-env) '())

  (define (extend-env x v env) (cons (list x v) env))

  (define (apply-env env x)
    (let ((probe (assq x env)))
      (if probe
          (cadr probe)
          #f)))
  
;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  (define extend-env*
    (lambda (vars vals env)
      (cond ((and (null? vars) (null? vals))
             env)
            ((or (null? vars) (null? vals))
             (eopl:error 'extend-env* "Mismatched arguments"))
            (else
             (extend-env* (cdr vars)
                          (cdr vals)
                          (extend-env (car vars) (car vals) env))))))

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Env

  ;; (init-env) builds an environment in which
  ;; true is bound to the expressed value #t,
  ;; false is bound to the expressed value #f,
  ;; and x is bound to the expressed value 10.

  (define init-env 
    (lambda ()
      (extend-env 
       'true (bool-val #t)
       (extend-env
        'false (bool-val #f)
        (extend-env
         'x (num-val 10)
         (empty-env))))))

  )
