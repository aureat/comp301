#lang eopl
(require eopl/tests/private/utils)

(require "data-structures.rkt")  ; for expval constructors
(require "lang.rkt")             ; for scan&parse
(require "interp.rkt")           ; for value-of-program

;; run : String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define equal-answer?
  (lambda (ans correct-ans)
    (equal? ans (sloppy->expval correct-ans))))

(define sloppy->expval 
  (lambda (sloppy-val)
    (cond
      ((number? sloppy-val) (num-val sloppy-val))
      ((boolean? sloppy-val) (bool-val sloppy-val))
      ((list? sloppy-val) (queue-val sloppy-val)) ; NOTE: Added
      (else
       (eopl:error 'sloppy->expval 
                   "Can't convert sloppy value to expval: ~s"
                   sloppy-val)))))

(define-syntax-rule (check-run (name str res) ...)
  (begin
    (cond [(eqv? 'res 'error)
           (check-exn always? (lambda () (run str)))]
          [else
           (check equal-answer? (run str) 'res (symbol->string 'name))])
    ...))

;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

(check-run  
 ;; simple arithmetic
 (positive-const "11" 11)
 (negative-const "-33" -33)
 (simple-arith-1 "-(44,33)" 11)
 
 ;; nested arithmetic
 (nested-arith-left "-(-(44,33),22)" -11)
 (nested-arith-right "-(55, -(22,11))" 44)
 
 ;; simple variables
 (test-var-1 "x" 10)
 (test-var-2 "-(x,1)" 9)
 (test-var-3 "-(1,x)" -9)
 
 ;; simple unbound variables
 (test-unbound-var-1 "foo" error)
 (test-unbound-var-2 "-(x,foo)" error)
 
 ;; simple conditionals
 (if-true "if zero?(0) then 3 else 4" 3)
 (if-false "if zero?(1) then 3 else 4" 4)
 
 ;; test dynamic typechecking
 (no-bool-to-diff-1 "-(zero?(0),1)" error)
 (no-bool-to-diff-2 "-(1,zero?(0))" error)
 (no-int-to-if "if 1 then 2 else 3" error)
 
 ;; make sure that the test and both arms get evaluated
 ;; properly. 
 (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
 (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
 
 ;; and make sure the other arm doesn't get evaluated.
 (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
 (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)
 
 ;; simple let
 (simple-let-1 "let x = 3 in x" 3)
 
 ;; make sure the body and rhs get evaluated
 (eval-let-body "let x = 3 in -(x,1)" 2)
 (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)
 
 ;; check nested let and shadowing
 (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
 (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
 (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)
 
 ;; simple applications
 (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
 (apply-simple-proc "let f = proc (x) -(x,1) in (f 30)" 29)
 (let-to-proc-1 "(proc(f)(f 30)  proc(x)-(x,1))" 29)


 ;##########################################################
 ; Main Project Tests

 (simple-queue "empty-queue()" ())

 (simple-queue-push-0 "queue-push(empty-queue(),0)" (0))
 (simple-queue-push-1 "queue-push(empty-queue(),8)" (8))
 (simple-queue-push-2 "queue-push(queue-push(empty-queue(),8), 13)" (13 8))
 (simple-queue-push-3 "queue-push(queue-push(queue-push(empty-queue(),56), 34), 1)" (1 34 56))

 (simple-queue-pop-0 "queue-pop(empty-queue())" ())
 (simple-queue-pop-1 "queue-pop(queue-push(empty-queue(),8))" ())
 (simple-queue-pop-2 "queue-pop(queue-push(queue-push(empty-queue(),8), 13))" (13))
 (simple-queue-pop-3 "queue-pop(queue-pop(queue-push(queue-push(empty-queue(),8), 13)))" ())
 (simple-queue-pop-4 "queue-pop(queue-pop(queue-pop(queue-push(queue-push(empty-queue(),8), 13))))" ())
 (simple-queue-pop-5 "queue-pop(queue-push(queue-push(queue-push(empty-queue(),56), 34), 1))" (1 34))
 

 (simple-queue-peek-0 "queue-peek(empty-queue())" 2000)
 (simple-queue-peek-1 "queue-peek(queue-push(empty-queue(),8))" 8)
 (simple-queue-peek-2 "queue-peek(queue-push(queue-push(empty-queue(),8), 13))" 8)
 (simple-queue-peek-3 "queue-peek(queue-pop(queue-push(queue-push(empty-queue(),8), 13)))" 13)
 (simple-queue-peek-4 "queue-peek(queue-pop(queue-pop(queue-push(queue-push(empty-queue(),8), 13))))" 2000)
 (simple-queue-peek-5 "queue-push(empty-queue(), queue-peek(queue-push(empty-queue(),1)))" (1))
 
 (simple-push-multi-0 "queue-push-multi(empty-queue())" ())
 (simple-push-multi-1 "queue-push-multi(empty-queue(),8,13,21)" (21 13 8))
 (simple-push-multi-2 "queue-push-multi(empty-queue(),1,2,3,4,5)" (5 4 3 2 1))
 (simple-push-multi-3 "queue-peek(queue-push-multi(empty-queue(),1,2,3,4,5))" 1)
 (simple-push-multi-4 "queue-pop(queue-push-multi(empty-queue(),1,2,3,4,5))" (5 4 3 2))

 (simple-pop-multi-0 "queue-pop-multi(empty-queue(), 1)" ())
 (simple-pop-multi-1 "queue-pop-multi(queue-push-multi(empty-queue(),1,2,3,4,5) , 2)" (5 4 3))
 (simple-pop-multi-2 "queue-pop-multi(queue-push-multi(empty-queue(),1,2,3,4,5) , 4)" (5))
 (simple-pop-multi-3 "queue-pop-multi(queue-push-multi(empty-queue(),1,2,3,4,5) , 5)" ())
 (simple-pop-multi-4 "queue-pop-multi(queue-push-multi(empty-queue(),1,2,3,4,queue-peek(queue-push(empty-queue(),5))), 3)" (5 4))


 (simple-merge-0 "queue-merge(queue-push-multi(empty-queue(), 1, 2, 3), queue-push-multi(empty-queue(), 4, 5, 6))" (6 5 4 3 2 1))
 (simple-merge-1 "queue-merge(queue-push(empty-queue(), 1), queue-pop-multi(queue-push-multi(empty-queue(),1,2,3,4,5) , 1))" (5 4 3 2 1))
 (simple-merge-2 "queue-merge(empty-queue(), queue-push-multi(empty-queue(), 4, 5, 6))" (6 5 4))
)

(display "If you don't see \"FAILURE\" all tests were successful. If not revise your code.")