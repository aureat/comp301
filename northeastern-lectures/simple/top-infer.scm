;; SIMPLE
;;
;; Tests for type inference.

(module top-infer (lib "eopl.ss" "eopl")
  
  ;; top level module.  Loads all required pieces and runs the test
  ;; suite.

  (require "drscheme-init.scm")
  (require "data-structures.scm")  ; for expval constructors
  (require "lang.scm")             ; for scan&parse
  (require "infer.scm")            ; for well-typed?
  (require "tests-infer.scm")      ; for test-list
  
  (provide (all-defined))
  (provide (all-from "infer.scm"))
  (provide (all-from "lang.scm"))
  
  ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;
  
  ;; run : string -> expval

  (define run
    (lambda (string)
      (well-typed? (scan&parse string))))
  
  ;; run-all : () -> unspecified

  ;; runs all the tests in test-list, comparing the results with
  ;; equal-answer?  

  (define run-all
    (lambda ()
      (run-tests! run equal-answer? test-list)))
  
  (define equal-answer?
    (lambda (ans correct-ans)
      (equal? ans correct-ans)))
    
  ;; (run-one sym) runs the test whose name is sym
  
  (define run-one
    (lambda (test-name)
      (let ((the-test (assoc test-name test-list)))
        (cond
          ((assoc test-name test-list)
           => (lambda (test)
                (run (cadr test))))
          (else (eopl:error 'run-one "no such test: ~s" test-name))))))
 
  (run-all)
  
  )

