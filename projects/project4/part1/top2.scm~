;#lang eopl
(module top2 (lib "eopl.ss" "eopl")
  
  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all).

  (require "top.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "interp.scm")
  (require "lang.scm")

  (provide run2)

  (define run2
    (lambda (string env)
      (vop (scan&parse string) env)))
  
  (define vop
    (lambda (pgm env)
      (cases program pgm
        (a-program (exp1)
                 (value-of exp1 env)))))
   
  
  )




