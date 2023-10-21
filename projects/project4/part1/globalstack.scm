(module globalstack (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (print-struct #t)
  ;(require "data-structures.scm")
  
  (provide initialize-allstacks!
           get-expstack get-envstack get-stostack
           get-expstack-as-list get-envstack-as-list
           add-to-expstack add-to-envstack add-to-stostack)
  
  ;;;;;;;;;;;;;;;; stacks to keep track of each call of value-of function ;;;;;;;;;;;;;;;;

  ; These stacks are used to store the snapshots of the system at each value-of function call. ;
  ;;expstack : stores each expression that is passed to value-of function as a parameter.
  ;;envstack : stores each environment that is passed to value-of function as a parameter.
  ;;stostack : stores the current the-store at each call of value-of function.
  (define expstack 'uninitialized)
  (define envstack 'uninitialized)
  (define stostack 'uninitialized)
  

  ;; empty-store : () -> Sto
  (define empty-stack
    (lambda () '()))
  
  ;; initialize-store! : () -> Sto
  ;; usage: (initialize-store!) sets each stack to empty list.
  (define initialize-allstacks!
    (lambda ()
      (set! expstack (empty-stack))
      (set! envstack (empty-stack))
      (set! stostack (empty-stack))))

  ;; get-[exp,env,sto]stack : () -> Sto
  ;; usage: (get-[exp,env,sto]stack)
  (define get-expstack
    (lambda () expstack))
  (define get-envstack
    (lambda () envstack))
  (define get-stostack
    (lambda () stostack))
  
  ;; add-to-[exp,env,sto]stack : ExpVal -> Unspecified
  ;; usage: (add-to-[exp,env,sto]stack [exp,env,sto]) 
  (define add-to-expstack
    (lambda (val) (set! expstack (append expstack (list val)))))
  (define add-to-envstack
    (lambda (val) (set! envstack (append envstack (list val)))))
  (define add-to-stostack
    (lambda (val) (set! stostack (append stostack (list val)))))

  
  (define get-expstack-as-list
     (lambda ()
       (letrec
         ((inner-loop
            ;; convert sto to list as if its car was location n
            (lambda (sto)
              (if (null? sto)
                '()
                (begin
                  (eopl:printf "~s~%" (car sto))
                  (inner-loop (cdr sto)))))))
         (inner-loop expstack))))
  
  (define get-envstack-as-list
     (lambda ()
        (pretty-print envstack)))

  )