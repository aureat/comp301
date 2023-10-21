(module tests mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
  
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
      (if-true "if =(0,0) then 3 else 4" 3)
      (if-false "if =(0,1) then 3 else 4" 4)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(=(0,0),1)" error)
      (no-bool-to-diff-2 "-(1,=(0,0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if =(0,-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "if =(0,-(11, 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if =(0,-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if =(0,-(11,12)) then foo else 4" 4)

      ;; translation of simple let
      (simple-let-1 "define f = proc (x) x
                    (f 3)"
         3)

      ;; make sure the bodies and operands get evaluated
      (eval-proc-body-1 "define f = proc (x) -(x,1)
                         (f 3)"
         2)

      (eval-proc-body-2 "define f = proc (x) -(x,1)
                         (f -(4,1))"
         2)

      ;; check shadowing
      (eval-proc-body-2 "define x = proc (x) -(x,1)
                         (x -(4,1))"
         2)

      ;; zero arguments
      (zero-arguments "define f = proc () 13
                       define g = proc () 14
                       *((f),(g))"
        182)

      ;; two or more arguments
      (two-arguments "define f = proc (x y) *(x,y)
                      define g = proc (a b) +(a,b)
                      (f (g 3 10) (g 7 7))"
        182)

      (fact-of-6  "define fact = proc (x) if =(0,x)
                                            then 1
                                            else *(x, (fact -(x,1)))
                   (fact 6)"
        720)

      (mutual-recursion
        "define even = proc (n) if =(0,n) then true else (odd -(n,1))
         define odd = proc (n) if =(0,n) then false else (even -(n,1))
         (odd 13)"
        #t)
      
      ))
  )
