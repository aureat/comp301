#lang eopl

;(require srfi/1)

;; interpreter for the PROC language, using the procedural
;; representation of procedures.



(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; run : String -> ExpVal
(define run
  (lambda (s)
    (value-of-program (scan&parse s))))

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      
      (const-exp (num) (num-val num))
      
      
      (var-exp (var) (apply-env env var))
      
     
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      
      
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      
     
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))

      
      (greater-exp (exp1 exp2)
                   (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                       (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                         (bool-val (> num1 num2)))))

      
       (less-exp (exp1 exp2)
                   (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                       (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                         (bool-val (< num1 num2)))))

      
       (equal-exp (exp1 exp2)
                   (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                       (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                         (bool-val (= num1 num2)))))
      
      
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;;For problem 1 implement my-cond procedure
      ;(my-cond     ......
       ;            .....
       ;            ......)

      (my-cond-exp (cond1 exp1 conds exps else-exp)
                   (let* ((cond-vals (map (lambda (exp) (value-of exp env)) (cons cond1 conds)))
                          (true-cond-vals (filter (lambda (pair) (expval->bool (car pair)))
                                                 (map cons cond-vals (cons exp1 exps)))))
                       (if (null? true-cond-vals)
                           (value-of else-exp env)
                           (value-of (cdr (last true-cond-vals)) env))))
                        
      ; Modify proc-exp and call-exp for Problem 2
      ; Hint: var is now a list, you need value of all of them
      
      (proc-exp (vars body)
                (proc-val (procedure vars body env)))
      
      (call-exp (rator rand)
                (let ((proc (expval->proc (value-of rator env)))
                      (args (map (lambda (arg) (value-of arg env)) rand)))
                  (apply-procedure proc args)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      )))

; Helpers

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (last lst) (list-ref lst (- (length lst) 1)))

; Old: apply-procedure : Proc * ExpVal -> ExpVal
; New: apply-procedure : Proc * ListOf(ExpVal) -> ExpVal
; Hint: You can use extend-env* (It's already implemented for you on environment.rkt).
(define (apply-procedure proc1 vals)
  (cases proc proc1
     (procedure (vars body saved-env)
                (value-of body (extend-env* vars vals saved-env)))))
