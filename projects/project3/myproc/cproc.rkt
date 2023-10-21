#lang eopl

;; test utils
(require eopl/tests/private/utils)

;; racket-ffi
(require ffi/unsafe)
(require ffi/unsafe/cvector)

;; extending myproc
(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

;; clib/queue library
(define clib/queue (ffi-lib "clib/bin/queue.so"))

;; clib/queue types
(define _Queue (_cpointer 'void))
(define _queue-pointer (_cpointer _Queue))

;; clib/queue functions
(define clib/queue/new
  (get-ffi-obj "queue_new" clib/queue (_fun _int -> _queue-pointer)))
(define clib/queue/empty?
  (get-ffi-obj "queue_isempty" clib/queue (_fun _queue-pointer -> _int)))
(define clib/queue/full?
  (get-ffi-obj "queue_isfull" clib/queue (_fun _queue-pointer -> _int)))
(define clib/queue/size
  (get-ffi-obj "queue_size" clib/queue (_fun _queue-pointer -> _int)))
(define clib/queue/capacity
  (get-ffi-obj "queue_capacity" clib/queue (_fun _queue-pointer -> _int)))
(define clib/queue/->list
  (get-ffi-obj "queue_tolist" clib/queue (_fun _queue-pointer -> _pointer)))
(define clib/queue/resize
  (get-ffi-obj "queue_resize" clib/queue (_fun _queue-pointer _int -> _int)))
(define clib/queue/push
  (get-ffi-obj "queue_push" clib/queue (_fun _queue-pointer _int -> _void)))
(define clib/queue/pop
  (get-ffi-obj "queue_pop" clib/queue (_fun _queue-pointer -> _int)))
(define clib/queue/peek
  (get-ffi-obj "queue_peek" clib/queue (_fun _queue-pointer -> _int)))
(define clib/queue/push-multi
  (get-ffi-obj "queue_push_multi" clib/queue (_fun _queue-pointer _pointer _int -> _void)))
(define clib/queue/pop-multi
  (get-ffi-obj "queue_pop_multi" clib/queue (_fun _queue-pointer _int -> _int)))
(define clib/queue/merge
  (get-ffi-obj "queue_merge" clib/queue (_fun _queue-pointer _queue-pointer -> _queue-pointer)))
(define clib/queue/free
  (get-ffi-obj "queue_free" clib/queue (_fun _queue-pointer -> _void)))

;; cqueue:new-with-capacity : Int -> CQueue
(define (cqueue:new-with-capacity capacity)
  (cqueue (clib/queue/new capacity)))

;; cqueue:new : Void -> CQueue
(define (cqueue:new)
  (cqueue:new-with-capacity 10))

;; cqueue:free : CQueue -> Void
(define (cqueue:free queue)
  (clib/queue/free (queue->ptr queue)))

;; cqueue:empty? : CQueue -> Bool
(define (cqueue:empty? queue)
  (if (= (clib/queue/empty? (queue->ptr queue))
         0)
      #f #t))

;; cqueue:size : CQueue -> Int
(define (cqueue:size queue)
  (clib/queue/size (queue->ptr queue)))

;; cqueue:capacity : CQueue -> Int
(define (cqueue:capacity queue)
  (clib/queue/capacity (queue->ptr queue)))

;; cqueue:resize : CQueue * Int -> CQueue
(define (cqueue:resize queue size)
  (clib/queue/resize (queue->ptr queue) size)
  queue)

;; cqueue:push : CQueue * Int -> CQueue
(define (cqueue:push queue num)
  (clib/queue/push (queue->ptr queue) num)
  queue)

;; cqueue:pop : CQueue -> Int
(define (cqueue:pop queue)
  (clib/queue/pop (queue->ptr queue)))

;; cqueue:peek : CQueue -> Int
(define (cqueue:peek queue)
  (clib/queue/peek (queue->ptr queue)))

;; cqueue:push-multi : CQueue * List<Int> -> CQueue
(define (cqueue:push-multi queue nums)
  (clib/queue/push-multi (queue->ptr queue) 
                         (int-list->cvector-ptr nums)
                         (length nums))
  queue)

;; cqueue:pop-multi : CQueue * Int -> CQueue
(define (cqueue:pop-multi queue count)
  (clib/queue/pop-multi (queue->ptr queue) count)
  queue)

;; cqueue:merge : CQueue * CQueue -> CQueue
(define (cqueue:merge q1 q2)
  (cqueue (clib/queue/merge (queue->ptr q1)
                            (queue->ptr q2))))


;; identifier?
(define identifier? symbol?)

;; data structures
(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (proc-val (proc proc?))
  (queue-val (queue queue?)))

(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)
   (saved-env environment?)))

(define-datatype queue queue?
  (cqueue (ptr cpointer?)))

;; expval->num : ExpVal -> Num
(define (expval->num v)
  (cases expval v
    (num-val (num) num)
    (else (extractor-error 'num-val v))))

;; expval->bool : ExpVal -> Bool
(define (expval->bool v)
  (cases expval v
    (bool-val (bool) bool)
    (else (extractor-error 'bool-val v))))

;; expval->queue : ExpVal -> Queue
(define (expval->queue v)
  (cases expval v
    (queue-val (queue) queue)
    (else (extractor-error 'queue-val v))))

;; expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (else (extractor-error 'proc-val v)))))

;; queue->ptr : Queue -> Ptr
(define (queue->ptr v)
  (cases queue v
    (cqueue (ptr) ptr)
    (else (extractor-error 'cqueue v))))

;; extractor-error : Symbol * DataType -> Void
(define extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

(define environment?
  (lambda (x)
    (or (empty-env-record? x)
        (and (pair? x)
             (symbol? (car (car x)))
             (expval? (cadr (car x)))
             (environment? (cdr x))))))

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> Env
;; usage: (init-env) = [x=10, y=302, z=304]
;; (init-env) builds an environment with the following bindings:
;; x = 10, y = 302, z = 304

;;; initialize environment with x, y, z
(define init-env 
  (lambda ()
    (extend-env
     'z (num-val 304)
     (extend-env
      'y (num-val 302)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; run : String -> ExpVal
(define run
  (lambda (s)
    (value-of-program (scan&parse s))))

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1) (value-of exp1 (init-env))))))

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

      (queue-exp () (queue-val (cqueue:new)))

      (queue-push-exp (exp1 exp2)
                      (let ((queue (expval->queue (value-of exp1 env)))
                            (num (expval->num (value-of exp2 env))))
                        (queue-val (cqueue:push queue num))))

      (queue-pop-exp (exp)
                     (let ((queue (expval->queue (value-of exp env))))
                        (queue-val
                         (if (queue-warn-empty? queue) queue
                             (begin (cqueue:pop queue) queue)))))

      (queue-peek-exp (exp)
                      (let ((queue (expval->queue (value-of exp env))))
                        (num-val (if (queue-warn-empty? queue) 2000
                                       (cqueue:peek queue)))))

      (queue-push-multi-exp (exp exps)
                            (let ((queue (expval->queue (value-of exp env)))
                                  (nums (map (lambda (exp) (expval->num (value-of exp env))) exps)))
                              (queue-val (cqueue:push-multi queue nums))))

      (queue-pop-multi-exp (exp1 exp2)
                            (let ((queue (expval->queue (value-of exp1 env)))
                                  (num (expval->num (value-of exp2 env))))
                              (queue-val (queue-pop-multi queue num))))

      (queue-merge-exp (exp1 exp2)
                        (let ((q1 (expval->queue (value-of exp1 env)))
                              (q2 (expval->queue (value-of exp2 env))))
                          (queue-val (cqueue:merge q1 q2))))
      
      )))

;; int-list->cvector-ptr : List<Int> -> Ptr
(define (int-list->cvector-ptr nums)
  (cvector-ptr (list->cvector nums _int)))

;; expval->cqueue->ptr : Expval -> Ptr
(define (expval->queue->ptr val)
  (queue->ptr (expval->queue val)))

;; ptr->queue->expval : Ptr -> ExpVal
(define (ptr->queue->expval ptr)
  (queue-val (cqueue ptr)))

;; queue-warn-empty? : CQueue -> Bool
(define (queue-warn-empty? queue)
  (if (cqueue:empty? queue)
      (begin (display "Warning: Empty Queue!")
             (newline)
             #t)
      #f))

;; queue-pop-multi : CQueue * Int -> Void
(define (queue-pop-multi queue num)
  (if (or (= num 0) (queue-warn-empty? queue)) queue
      (queue-pop-multi (begin (cqueue:pop queue) queue)
                       (- num 1))))

;; cqueue->list : CQueue -> List<Int>
(define (queue->list queue)
  (if (cqueue:empty? queue) '()
      (cvector->list (make-cvector* (clib/queue/->list (queue->ptr queue))
                                    _int
                                    (cqueue:size queue)))))

;;-----------------------------------------

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env))))))

(define equal-answer?
  (lambda (ans correct-ans)
    (cases expval ans
      (queue-val (queue) (equal? (reverse (queue->list queue)) correct-ans))
      (else (equal? ans (sloppy->expval correct-ans))))))

(define sloppy->expval
  (lambda (sloppy-val)
    (cond
      ((number? sloppy-val) (num-val sloppy-val))
      ((boolean? sloppy-val) (bool-val sloppy-val))
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

;  ;##########################################################
;  ; Main Project Tests

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