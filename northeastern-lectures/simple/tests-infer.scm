; Tests of type inference for SIMPLE.

(module tests-infer mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
  
      ;; simple arithmetic
      (positive-const "11" #t)
      (negative-const "-33" #t)
      (simple-arith-1 "-(44,33)" #t)
  
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" #t)
      (nested-arith-right "-(55, -(22,11))" #t)
  
      ;; simple variables
      (test-var-1 "x" #t)
      (test-var-2 "-(x,1)" #t)
      (test-var-3 "-(1,x)" #t)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" #f)
      (test-unbound-var-2 "-(x,foo)" #f)
  
      ;; simple conditionals
      (if-true "if =(0,0) then 3 else 4" #t)
      (if-false "if =(0,1) then 3 else 4" #t)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(=(0,0),1)" #f)
      (no-bool-to-diff-2 "-(1,=(0,0))" #f)
      (no-int-to-if "if 1 then 2 else 3" #f)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if =(0,-(11,11)) then 3 else 4" #t)
      (if-eval-test-false "if =(0,-(11, 12)) then 3 else 4" #t)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if =(0,-(11, 11)) then 3 else foo" #f)
      (if-eval-test-false-2 "if =(0,-(11,12)) then foo else 4" #f)

      ;; translation of simple let
      (simple-let-1 "define f = proc (x) x
                     (f 3)"
         #t)

      ;; make sure the bodies and operands get evaluated
      (eval-proc-body-1 "define f = proc (x) -(x,1)
                         (f 3)"
         #t)

      (eval-proc-body-2 "define f = proc (x) -(x,1)
                         (f -(4,1))"
         #t)

      ;; check shadowing
      (eval-proc-body-2 "define x = proc (x) -(x,1)
                         (x -(4,1))"
         #t)

      ;; zero arguments
      (zero-arguments "define f = proc () 13
                       define g = proc () 14
                       *((f),(g))"
        #t)

      ;; two or more arguments
      (two-arguments "define f = proc (x y) *(x,y)
                      define g = proc (a b) +(a,b)
                      (f (g 3 10) (g 7 7))"
        #t)

      (fact-of-6  "define fact = proc (x) if =(0,x)
                                            then 1
                                            else *(x, (fact -(x,1)))
                   (fact 6)"
        #t)

      (mutual-recursion
        "define even = proc (n) if =(0,n) then true else (odd -(n,1))
         define odd = proc (n) if =(0,n) then false else (even -(n,1))
         (odd 13)"
        #t)

      (mutual-recursion-wrong1
        "define even = proc (n) if =(0,n) then true else (odd -(n,1))
         define odd = proc (n) if =(0,n) then false else (even -(n,1) 31)
         (odd 13)"
        #f)
      
      (mutual-recursion-wrong2
        "define even = proc (n) if =(0,n) then true else (odd -(n,1))
         define odd = proc (n) if =(0,n) then false else (even -(n,1))
         (odd even)"
        #f)
      
      (mutual-recursion-wrong3
        "define even = proc (n) if =(0,n) then true else (odd -(n,1))
         define odd = proc (n) if -(0,n) then false else (even -(n,1))
         (odd 13)"
        #f)
      
      (mutual-recursion-wrong4
        "define even = proc (n) if =(0,n) then true else (odd =(n,1))
         define odd = proc (n) if =(0,n) then false else (even -(n,1))
         (odd 13)"
        #f)
      
      (higher-order
       "define search = proc (x f i j)
                          if >(i,j)
                             then -1
                             else if =(x,(f i))
                                     then i
                                     else (search x f +(i,1) j)
        define g = proc (square x a b c)
                     +(*(a,(square x)),+(*(b,x),c))
        define square = proc (x) *(x,x)
        define h = proc (x) (g square x 5 -13 7)
        (search 1231 h 0 25)"
       #t)
                           
      (higher-order-wrong1
       "define search = proc (x f i j)
                          if >(i,j)
                             then -1
                             else if =(x,(f i))
                                     then i
                                     else (search x f +(i,1) j)
        define g = proc (square x a b c)
                     +(*(a,(square x)),+(*(b,x),c))
        define square = proc (x) *(x,x)
        define h = proc (x) (g square x 5 -13 7)
        (search h 0 25)"
       #f)
                           
      (higher-order-wrong2
       "define search = proc (x f i j)
                          if >(i,j)
                             then -1
                             else if =(x,(f i))
                                     then i
                                     else (search x f +(i,1) j)
        define g = proc (square x a b c)
                     +(*(a,(square x)),+(*(b,x),c))
        define square = proc (x) *(x,x)
        define h = proc (x) (g square x 5 -13 7)
        (search h 1231 0 25)"
       #f)
                           
      (higher-order-wrong3
       "define search = proc (x f i j)
                          if >(i,j)
                             then -1
                             else if =(x,(f i))
                                     then i
                                     else (search x f +(i,1) j)
        define g = proc (square x a b c)
                     +(*(a,(square x)),+(*(b,x),c))
        define square = proc (x) *(x,x)
        define h = proc (x) (g g x 5 -13 7)
        (search 1231 h 0 25)"
       #f)
                           
      (higher-order-wrong4
       "define search = proc (x f i j)
                          if >(i,j)
                             then -1
                             else if =(x,(f i))
                                     then i
                                     else (search x g +(i,1) j)
        define g = proc (square x a b c)
                     +(*(a,(square x)),+(*(b,x),c))
        define square = proc (x) *(x,x)
        define h = proc (x) (g square x 5 -13 7)
        (search 1231 h 0 25)"
       #f)
                           
      (higher-order-wrong5
       "define search = proc (x f i j)
                          if >(i,j)
                             then -1
                             else if =(x,(f i i))
                                     then i
                                     else (search x f +(i,1) j)
        define g = proc (square x a b c)
                     +(*(a,(square x)),+(*(b,x),c))
        define square = proc (x) *(x,x)
        define h = proc (x) (g square x 5 -13 7)
        (search 1231 h 0 25)"
       #f)
                           

      ))
  )
