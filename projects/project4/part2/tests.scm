(module tests mzscheme

  (require "drscheme-init.scm")
  (require "data-structures.scm")  ; for expval constructors
  (require "lang.scm")             ; for scan&parse
  (require "interp.scm")           ; for value-of-program
  (require "translator.scm")	   ; for translation-of-program

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  ;; simple arithmetic;;
  (display "positive-const  -->  ")
  (display(translation-of-program
           (scan&parse "11")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:const-exp 11))
  
  (display "negative-const  -->  ")
  (display (translation-of-program
            (scan&parse "-33")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:const-exp -33))

  ;; nested arithmetic;;
  (display "nested-arith-left  -->  ")
  (display (translation-of-program
            (scan&parse "-(-(44,33),22)")))
  (newline)
  (newline)
  ;Prints:
;  #(struct:a-program
;    #(struct:diff-exp #(struct:diff-exp #(struct:const-exp 44) #(struct:const-exp 33)) #(struct:const-exp 22)))
 
  (display "nested-arith-right  -->  ")
  (display (translation-of-program
            (scan&parse "-(55, -(22,11))")))
  (newline)
  (newline)
  ;Prints:
  ;   #(struct:a-program
;     #(struct:diff-exp #(struct:const-exp 55) #(struct:diff-exp #(struct:const-exp 22) #(struct:const-exp 11))))
   
  
  ;; simple conditionals;;
    
  (display "if-true  -->  ")
  (display (translation-of-program
            (scan&parse "if zero?(0) then 3 else 4")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:if-exp #(struct:zero?-exp #(struct:const-exp 0)) #(struct:const-exp 3) #(struct:const-exp 4)))

  (display "if-false  -->  ")
  (display (translation-of-program
            (scan&parse "if zero?(1) then 3 else 4")))
  (newline)
  (newline)
  ;Prints:
  ; #(struct:a-program #(struct:if-exp #(struct:zero?-exp #(struct:const-exp 1)) #(struct:const-exp 3) #(struct:const-exp 4)))
  
  ;; test dynamic typechecking;;
  (display "no-bool-to-diff-1  -->  ")
  (display (translation-of-program
            (scan&parse "-(zero?(0),1)")))
  (newline)
  (newline)
  ;Prints:
  ; #(struct:a-program #(struct:diff-exp #(struct:zero?-exp #(struct:const-exp 0)) #(struct:const-exp 1)))
  
  (display "if-eval-test-true  -->  ")
  (display (translation-of-program
            (scan&parse "if zero?(-(11,11)) then 3 else 4")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:if-exp #(struct:zero?-exp #(struct:diff-exp #(struct:const-exp 11) #(struct:const-exp 11))) #(struct:const-exp 3) #(struct:const-exp 4)))
  
  (display "if-eval-test-false  -->  ")
  (display (translation-of-program
            (scan&parse "if zero?(-(11, 12)) then 3 else 4")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:if-exp #(struct:zero?-exp #(struct:diff-exp #(struct:const-exp 11) #(struct:const-exp 12))) #(struct:const-exp 3) #(struct:const-exp 4)))
      
  ;; simple let;;
  (display "simple-let-1  -->  ")
  (display (translation-of-program
            (scan&parse "let x = 3 in x")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:let-exp x1 #(struct:const-exp 3) #(struct:var-exp x1)))
  
  (display "eval-let-body  -->  ")
  (display (translation-of-program
            (scan&parse "let x = 3 in -(x,1)")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:let-exp x1 #(struct:const-exp 3) #(struct:diff-exp #(struct:var-exp x1) #(struct:const-exp 1))))
  
  (display "eval-let-rhs  -->  ")
  (display (translation-of-program
            (scan&parse "let x = -(4,1) in -(x,1)")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:let-exp x1 #(struct:diff-exp #(struct:const-exp 4) #(struct:const-exp 1)) #(struct:diff-exp #(struct:var-exp x1) #(struct:const-exp 1))))

  ;; check nested let and shadowing;;
  (display "simple-nested-let  -->  ")
  (display (translation-of-program
            (scan&parse "let x = 3 in let y = 4 in -(x,y)")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:let-exp x1 #(struct:const-exp 3) #(struct:let-exp y1 #(struct:const-exp 4) #(struct:diff-exp #(struct:var-exp x1) #(struct:var-exp y1)))))

  (display "check-shadowing-in-body  -->  ")
  (display (translation-of-program
            (scan&parse "let x = 3 in let x = 4 in x")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:let-exp x1 #(struct:const-exp 3) #(struct:let-exp x2 x has been reinitialized. x2 created and shadows x1 #(struct:const-exp 4) #(struct:var-exp x2))))
  
  (display "check-shadowing-in-rhs  -->  ")
  (display (translation-of-program
            (scan&parse "let x = 3 in let x = -(x,1) in x")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:let-exp x1 #(struct:const-exp 3) #(struct:let-exp x2 x has been reinitialized. x2 created and shadows x1 #(struct:diff-exp #(struct:var-exp x1) #(struct:const-exp 1)) #(struct:var-exp x2))))

  
  ;; simple applications;;
  (display "apply-proc-in-rator-pos  -->  ")
  (display (translation-of-program
            (scan&parse "(proc(x) -(x,1)  30)")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:call-exp #(struct:proc-exp x1 #(struct:diff-exp #(struct:var-exp x1) #(struct:const-exp 1))) #(struct:const-exp 30)))
  
  (display "apply-simple-proc  -->  ")
  (display (translation-of-program
            (scan&parse "let f = proc (x) -(x,1) in (f 30)")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:let-exp f1 #(struct:proc-exp x1 #(struct:diff-exp #(struct:var-exp x1) #(struct:const-exp 1))) #(struct:call-exp #(struct:var-exp f1) #(struct:const-exp 30))))
  
  (display "let-to-proc-1  -->  ")
  (display (translation-of-program
            (scan&parse "(proc(f)(f 30)  proc(x)-(x,1))")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:call-exp #(struct:proc-exp f1 #(struct:call-exp #(struct:var-exp f1) #(struct:const-exp 30))) #(struct:proc-exp x1 #(struct:diff-exp #(struct:var-exp x1) #(struct:const-exp 1)))))
  
  (display "nested-procs  -->  ")
  (display (translation-of-program
            (scan&parse "((proc (x) proc (y) -(x,y)  5) 6)")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:call-exp #(struct:call-exp #(struct:proc-exp x1 #(struct:proc-exp y1 #(struct:diff-exp #(struct:var-exp x1) #(struct:var-exp y1)))) #(struct:const-exp 5)) #(struct:const-exp 6)))
  
  (display "nested-procs2  -->  ")
  (display (translation-of-program
            (scan&parse "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:let-exp f1 #(struct:proc-exp x1 #(struct:proc-exp y1 #(struct:diff-exp #(struct:var-exp x1) #(struct:var-exp y1)))) #(struct:call-exp #(struct:call-exp #(struct:var-exp f1) #(struct:diff-exp #(struct:const-exp 10) #(struct:const-exp 5))) #(struct:const-exp 6))))
  
  (display "y-combinator-1  -->  ")
  (display (translation-of-program
            (scan&parse "let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in let
    t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)")))
  (newline)
  (newline)
  ;Prints:
  ;#(struct:a-program #(struct:let-exp fix1 #(struct:proc-exp f1 #(struct:let-exp d1 #(struct:proc-exp x1 #(struct:proc-exp z1 #(struct:call-exp #(struct:call-exp #(struct:var-exp f1) #(struct:call-exp #(struct:var-exp x1) #(struct:var-exp x1))) #(struct:var-exp z1)))) #(struct:proc-exp n1 #(struct:call-exp #(struct:call-exp #(struct:var-exp f1) #(struct:call-exp #(struct:var-exp d1) #(struct:var-exp d1))) #(struct:var-exp n1))))) #(struct:let-exp t4m1 #(struct:proc-exp f1 #(struct:proc-exp x1 #(struct:if-exp #(struct:zero?-exp #(struct:var-exp x1)) #(struct:const-exp 0) #(struct:diff-exp #(struct:call-exp #(struct:var-exp f1) #(struct:diff-exp #(struct:var-exp x1) #(struct:const-exp 1))) #(struct:const-exp -4))))) #(struct:let-exp times41 #(struct:call-exp #(struct:var-exp fix1) #(struct:var-exp t4m1)) #(struct:call-exp #(struct:var-exp times41) #(struct:const-exp 3))))))

  (display "let-shadowing1 --> ")
  (display (translation-of-program
            (scan&parse "let x = 10 in let x = 10 in (proc (x) -(x,3) 4)")))

  (newline)
  (newline)
  ;;#(struct:a-program #(struct:let-exp x1 #(struct:const-exp 10) #(struct:let-exp x2 x has been reinitialized. x2 is created and shadows x1. #(struct:const-exp 10) #(struct:call-exp #(struct:proc-exp x3 x has been reinitialized. x3 is created and shadows x2. #(struct:diff-exp #(struct:var-exp x3) #(struct:const-exp 3))) #(struct:const-exp 4)))))


  (display "let-shadowing2 --> ")
  (display (translation-of-program
            (scan&parse "let x = 5 in let y = 10 in let y = 5 in let z = 4 in (proc (x) -(x,3) 4)")))

  (newline)
  (newline)
  ;; #(struct:a-program #(struct:let-exp x1 #(struct:const-exp 5) #(struct:let-exp y1 #(struct:const-exp 10) #(struct:let-exp y2 y has been reinitialized. y2 is created and shadows y1. #(struct:const-exp 5) #(struct:let-exp z1 #(struct:const-exp 4) #(struct:call-exp #(struct:proc-exp x2 x has been reinitialized. x2 is created and shadows x1. #(struct:diff-exp #(struct:var-exp x2) #(struct:const-exp 3))) #(struct:const-exp 4)))))))

  )

