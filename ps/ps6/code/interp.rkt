#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      ;;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
      (const-exp (num) (num-val num))
      
      ;;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
      (var-exp (var) (apply-env env var))
      
      ;;\commentbox{\diffspec}
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      
      ;;\commentbox{\zerotestspec}
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      
      ;;\commentbox{\ma{\theifspec}}
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      
      ;;\commentbox{\ma{\theletspecsplit}}
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;;;;;;;;;;;;;;;;;;;;;;;;;; PS 6 ;;;;;;;;;;;;;;;;;;
      ;;;;; implement value-of for minus, double, gcd and prime? operations ;;;;;
      
      (minus-exp (exp)
                 (let ((val (value-of exp env)))
                   (let ((num (expval->num val)))
                     (num-val (- num)))))

      (double-exp (exp)
                 (let ((val (value-of exp env)))
                   (let ((num (expval->num val)))
                     (num-val (* 2 num)))))

      (gcd-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (calc-gcd num1 num2)))))

      (prime?-exp (exp)
                 (let ((val (value-of exp env)))
                   (let ((num (expval->num val)))
                     (bool-val (test-prime num)))))
      
      )))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;; write your helper procedures here;;;;;;;;;;;


(define (test-prime x)
  (define (divides? a b) (= (modulo a b) 0))
  (define root (sqrt x))
  (define (try i)
    (cond ((> i root) #t)
          ((divides? x i) #f)
          (else (try (+ i 1)))))
  (cond ((= x 2) #t)
        ((< x 2) #f)
        (else (try 2))))

(define (calc-gcd a b)
	(if (= b 0) a
            (calc-gcd b (remainder a b))))
   

      
