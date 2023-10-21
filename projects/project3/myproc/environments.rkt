#lang eopl

;; builds environment interface, using data structures defined in
;; data-structures.scm. 

(require "data-structures.rkt")

(provide init-env empty-env extend-env apply-env extend-env*)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> Env
;; usage: (init-env) = [x=10, y=302, z=304]
;; (init-env) builds an environment with the following bindings:
;; x = 10, y = 302, z = 304

;;; initialize environment with x, y, z
(define init-env 
  (lambda ()
    (extend-env
     'z (num-val 304)
     (extend-env
      'y (num-val 302)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

(define empty-env
  (lambda ()
    (empty-env-record)))

(define empty-env? 
  (lambda (x)
    (empty-env-record? x)))

(define extend-env
  (lambda (sym val old-env)
    (extended-env-record sym val old-env)))

(define apply-env
  (lambda (env search-sym)
    (if (empty-env? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let ((sym (extended-env-record->sym env))
              (val (extended-env-record->val env))
              (old-env (extended-env-record->old-env env)))
          (if (eqv? search-sym sym)
              val
              (apply-env old-env search-sym))))))

;; extend-env* : Listof(Var) * Listof(ExpVal) * Env -> Env
;; usage: (extend-env* (var1 ... vark) (val1 ... valk) [f]) = [g]
;;        where g(var) = vali    if var = vari for some i such that 1 <= i <= k
;;                     | f(var)  otherwise
(define extend-env*
  (lambda (var-list val-list env)
    (if (null? var-list)
        env
        (let ((var (car var-list))
              (val (car val-list)))
          (extend-env* (cdr var-list) (cdr val-list)
                       (extend-env var val env))))))

