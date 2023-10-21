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
      
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))
      
      (op-exp (exp1 exp2 op)
              (let ((num1 (expval->rational (value-of exp1 env)))
                    (num2 (expval->rational (value-of exp2 env))))
                (cond ((and (number? num1) (number? num2))
                        (num-val (ops-num-num num1 num2 op)))
                      ((and (number? num1) (not (number? num2)))
                        (rational-val (ops-num-rat num1 num2 op)))
                      ((and (number? num2) (not (number? num1)))
                        (rational-val (ops-rat-num num2 num1 op)))
                      (else
                        (rational-val (ops-rat-rat num1 num2 op))))))

      (zero?-exp (exp1)
                 (let ((num1 (expval->rational (value-of exp1 env))))
                      (if (number? num1)
                        (if (zero? num1) (bool-val #t) (bool-val #f))
                        (if (zero? (car num1)) (bool-val #t) (bool-val #f)))))

      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      (if-exp (cond1 exp1 conds exps else-exp)
              (let ((val1 (value-of cond1 env)))
                (if (expval->bool val1) 
                    (value-of exp1 env)
                    (if (null? conds) 
                        (value-of else-exp env)
                        (value-of 
                          (if-exp (car conds) (car exps) (cdr conds) (cdr exps) else-exp) 
                          env)))))

      (sum-exp (exp)
                (let ((lst (expval->list (value-of exp env))))
                  (num-val (sum-list-nums lst))))

      (rational-exp (num1 num2)
                    (if (zero? num2)
                        (eopl:error 'rational-exp "Denominator cannot be zero")
                        (rational-val (cons num1 num2))))

      (simpl-exp (exp1)
                 (let ((num (expval->rational (value-of exp1 env))))
                    (if (number? num) (num-val num)
                        (let* ((numer (car num)) (denom (cdr num))
                               (gcd (euclid-gcd numer denom)))
                              (rational-val (cons (/ numer gcd) (/ denom gcd)))))))   

      (new-list-exp () (list-of-nums-val '()))

      (cons-exp (exp1 exp2)
                (let ((num (expval->num (value-of exp1 env)))
                      (lst (expval->list (value-of exp2 env))))
                  (list-of-nums-val (cons (num-val num) lst))))

      )))


;;;;;;;;;;;;;;;; helper functions ;;;;;;;;;;;;;;;;

;; Euclid's algorithm for finding the greatest common divisor
;; in O(log(min(a, b))) time
(define (euclid-gcd a b) (if (zero? b) a (gcd b (remainder a b))))

;; Sum the numbers in a list of ExpVals
(define (sum-list-nums lst)
  (if (null? lst) 0 (+ (expval->num (car lst)) (sum-list-nums (cdr lst)))))

; ops-num-num : Num x Num x Int -> Num
(define (ops-num-num num1 num2 op)
  (cond ((= op 1) (+ num1 num2))
        ((= op 2) (* num1 num2))
        ((= op 3) (/ num1 num2))
        (else (- num1 num2))))

; ops-num-rat : Num x Rat x Int -> Rat
(define (ops-num-rat num rat op)
  (let ((numer (car rat)) (denom (cdr rat)))
    (cond ((= op 1) (cons (+ (* num denom) numer) denom))
          ((= op 2) (cons (* num numer) denom))
          ((= op 3) (cons (* num denom) numer))
          (else (cons (- (* num denom) numer) denom)))))

; ops-rat-num : Rat x Num x Int -> Rat
(define (ops-rat-num num rat op)
  (let ((numer (car rat)) (denom (cdr rat)))
    (cond ((= op 1) (cons (+ (* num denom) numer) denom))
          ((= op 2) (cons (* num numer) denom))
          ((= op 3) (cons numer (* num denom)))
          (else (cons (- numer (* num denom)) denom)))))

; ops-rat-rat : Rat x Rat x Int -> Rat
(define (ops-rat-rat rat1 rat2 op)
  (let ((numer1 (car rat1)) (numer2 (car rat2))
        (denom1 (cdr rat1)) (denom2 (cdr rat2)))
    (cond ((= op 1) (cons (+ (* numer1 denom2) (* denom1 numer2)) (* denom1 denom2)))
          ((= op 2) (cons (* numer1 numer2) (* denom1 denom2)))
          ((= op 3) (cons (* numer1 denom2) (* denom1 numer2)))
          (else (cons (- (* numer1 denom2) (* denom1 numer2)) (* denom1 denom2))))))