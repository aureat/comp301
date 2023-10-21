#lang eopl

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
      
      
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
      
      (proc-exp (var body)
                (proc-val (procedure var body env)))
      
      (call-exp (rator rand)
                (let ((proc (expval->proc (value-of rator env)))
                      (arg (value-of rand env)))
                  (apply-procedure proc arg)))
      
      ;;----------------------------------------------------
      ; INSERT YOUR CODE HERE
      ; Write the required expressions starting from here

      ;;-------------------------------------------------

      (queue-exp () (queue-val (empty-queue)))

      (queue-push-exp (exp1 exp2)
                      (let ((queue (expval->queue (value-of exp1 env)))
                            (num (expval->num (value-of exp2 env))))
                        (queue-val (queue-push queue num))))

      (queue-pop-exp (exp)
                     (let ((queue (expval->queue (value-of exp env))))
                        (queue-val (if (queue-warn-empty? queue) queue
                                       (queue-pop queue)))))

      (queue-peek-exp (exp)
                      (let ((queue (expval->queue (value-of exp env))))
                        (num-val (if (queue-warn-empty? queue) 2000
                                       (queue-peek queue)))))

      (queue-push-multi-exp (exp exps)
                            (let ((queue (expval->queue (value-of exp env)))
                                  (nums (map (lambda (exp) (expval->num (value-of exp env))) exps)))
                              (queue-val (queue-push-multi queue nums))))

      (queue-pop-multi-exp (exp1 exp2)
                            (let ((queue (expval->queue (value-of exp1 env)))
                                  (num (expval->num (value-of exp2 env))))
                              (queue-val (queue-pop-multi queue num))))

      (queue-merge-exp (exp1 exp2)
                        (let ((q1 (expval->queue (value-of exp1 env)))
                              (q2 (expval->queue (value-of exp2 env))))
                          (queue-val (queue-merge q1 q2))))
      
      )))

;;-----------------------------------------
; INSERT YOUR CODE HERE
; you may use this area to define helper functions
;;-----------------------------------------

; empty-queue : Void -> List<Int>
(define (empty-queue) '())

; queue-empty? : List<Int> -> Bool
(define (queue-empty? queue) (null? queue))

; queue-warn-empty? : List<Int> -> Bool
(define (queue-warn-empty? queue)
  (if (queue-empty? queue)
      (begin (display "Warning: Empty Queue!")
             (newline)
             #t)
      #f))

; queue-push : List<Int> * Int -> List<Int>
(define (queue-push queue num) (cons num queue))

; queue-pop : List<Int> -> List<Int>
(define (queue-pop queue)
  (let loop ((queue queue))
    (if (null? (cdr queue)) (empty-queue)
        (cons (car queue) (loop (cdr queue))))))

; queue-peek : List<Int> -> Int
(define (queue-peek queue)
  (let loop ((queue queue))
    (if (null? (cdr queue)) 
        (car queue)
        (loop (cdr queue)))))

; queue-push-multi : List<Int> * List<Int> -> List<Int>
(define (queue-push-multi queue nums)
  (if (null? nums) queue
      (queue-push-multi (queue-push queue (car nums)) (cdr nums))))

; queue-pop-multi : List<Int> * Num -> List<Int>
(define (queue-pop-multi queue num)
  (if (or (zero? num) (queue-warn-empty? queue))
      queue
      (queue-pop-multi (queue-pop queue) (- num 1))))

; queue-merge : List<Int> * List<Int> -> List<Int>
(define (queue-merge queue1 queue2)
  (let loop ((queue1 queue1) (queue2 queue2))
    (if (queue-empty? queue2) queue1
        (loop (queue-push queue1 (queue-peek queue2)) 
              (queue-pop queue2)))))


;;-----------------------------------------

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env))))))
