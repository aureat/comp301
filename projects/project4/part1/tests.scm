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

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; NO PROCEDURE CALLS UNTIL HERE!
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; simple applications
      (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
      ;; Recursive Print:
      ;; anonym --> 1

      (apply-simple-proc "let func = proc (x) -(x,1) in (func 30)" 29)
      ;; Recursive Print:
      ;; func --> 1

      (nested-procs "((proc (x) proc (y) -(x,y)  5) 6)" -1)
      ;; Recursive Print:
      ;; anonym --> 1
      ;; anonym --> 1

      (nested-procs2 "let g = proc(x) proc (y) -(x,y) in ((g -(10,5)) 6)" -1)
      ;; Recursive Print:
      ;; g --> 1
      ;; anonym --> 1
      

      (simple-letrec-1 "letrec minus(x) = -(x,1) in (minus 33)" 32)
      ;; Recursive Print:
      ;; minus --> 1

      (simple-letrec-2
        "letrec double(x) = if zero?(x)  
          then 0 else -((double -(x,1)), -2) in (double 4)"
        8)
      ;; Recursive Print:
      ;; double --> 1
      ;; ....double --> 2
      ;; ........double --> 3
      ;; ............double --> 4
      ;; ................double --> 5

      (simple-letrec-3
        "let m = -5 
          in letrec times-m(x) = if zero?(x) 
          then 0 else -((times-m -(x,1)), m) in (times-m 4)"
        20)
      ;; Recursive Print:
      ;; times-m --> 1
      ;; ....times-m --> 2
      ;; ........times-m --> 3
      ;; ............times-m --> 4
      ;; ................times-m --> 5

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;; EXTRA TEST CASES
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (nested-procs-3
       "letrec fibonacci(n) = if zero?(n) then 0 else if zero?(-(n, 1)) then 1 else (fibonacci -(n, 1)) in (fibonacci 6)"
       -2)

      (nested-letrecs-1
       "let m = proc(n) letrec double(x) = if zero?(x) then 0 else -(x, (double -(x, 1))) in (double n) in (m 4)"
       24)

      (nested-letrecs-2
"let a = 0 in let b = -1 in let c = 5 in let d = 10 in let e = 15 in
  letrec f1(func1) = ((func1 c) b) in 
    letrec f2(func2) = ((func2 a) b) in 
      letrec f3(func3) = ((func3 c) a) in
        letrec f4(func4) = ((func4 e) d) in
          letrec f5(func5) = ((func5 a) e) in
            let u1 = (f1 proc(a) proc(b) -(a, b)) in 
              let u2 = (f2 proc(a) proc(b) -(-(a, b), -10)) in 
                let u3 = (f3 proc(a) proc(b) -(-(a, b), -5)) in
                  let u4 = (f4 proc(a) proc(b) -(a, b)) in 
                    let u5 = (f5 proc(a) proc(b) -(-(a, b), -15)) in
                      letrec sum(x) = if zero?(x) then 0 else -(x, -(0, (sum -(x, 1)))) in 
                        letrec increment(x) = if zero?(x) then 1 else -((increment -(x, 1)), -1) in 
                          letrec double(x) = if zero?(x) then 0 else -((double -(x, 1)), -2) in
                            letrec triple(x) = if zero?(x) then 0 else -((triple -(x, 1)), -3) in
                              letrec quadruple(x) = if zero?(x) then 0 else -((quadruple -(x, 1)), -4) in
                                -((increment u1), -((sum u2), -((double u3), -((triple u4), (quadruple u5)))))"
      -39)
      ))
  )