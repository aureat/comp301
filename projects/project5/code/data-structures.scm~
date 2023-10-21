(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?
  (require "store.scm")                 ; for reference?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference. 

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    (ref-val
      (ref reference?))
    
    ; #####################################################
    ; ###### ENTER YOUR CODE HERE
    ; ###### add a new value type for your vectors (and possible for queues)
    ; #####################################################

    (vec-val
      (vec vec?))

    (queue-val
      (queue queue?))
    
    ; #####################################################
    
    )

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval->ref
    (lambda (v)
      (cases expval v
	(ref-val (ref) ref)
	(else (expval-extractor-error 'reference v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

  ;; HINT if you need extractors, add them here

  ; expval->vec : ExpVal -> Vec
  (define expval->vec
    (lambda (v)
      (cases expval v
        (vec-val (vec) vec)
        (else (expval-extractor-error 'vec v)))))

  ; expval->queue : ExpVal -> Queue
  (define expval->queue
    (lambda (v)
      (cases expval v
        (queue-val (queue) queue)
        (else (expval-extractor-error 'queue v)))))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ; #####################################################
  ; ###### ENTER YOUR CODE HERE
  ; ###### you might want to add a new datatype for vectors here similar 
  ; ###### to mutable pairs.
  ; #####################################################

  (define-datatype vec vec?
    (a-vector
     (head reference?)
     (length integer?)))

  (define-datatype queue queue?
    (a-queue
     (data vec?)
     (front reference?)
     (rear reference?)
     (size reference?)))

  ; #####################################################

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))
  
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec*
      (proc-names (list-of symbol?))
      (b-vars (list-of symbol?))
      (proc-bodies (list-of expression?))
      (saved-env environment?)))

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	(empty-env () '())
	(extend-env (sym val saved-env)
	  (cons
	    (list sym (expval->printable val))
	    (env->list saved-env)))
	(extend-env-rec* (p-names b-vars p-bodies saved-env)
	  (cons
	    (list 'letrec p-names '...)
	    (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
	(proc-val (p)
	  (cases proc p
	    (procedure (var body saved-env)
	      (list 'procedure var '... (env->list saved-env)))))
	(else val))))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE

  ; #####################################################
  ; Part A.
  ; #####################################################

  ; vec-new : Int x ExpVal -> Vec
  (define (vec-new length value)
    (if (> length 0)
        (let loop ((i 0) (ref -1))
          (if (= i length)
              (a-vector (- ref (- length 1)) length)
              (loop (+ i 1) (newref value))))
        (eopl:error 'vec-new "Length must be greater than 0")))

  ; vec-zeros : Int -> Vec
  (define (vec-zeros length)
    (vec-new length (num-val 0)))

  ; vec-set! : Vec x Int x ExpVal -> Void
  (define (vec-set! vector index value)
    (cases vec vector
      (a-vector (head length)
        (if (and (>= index 0) (< index length))
            (setref! (+ head index) value)
            (eopl:error 'vec-set! "Index out of bounds")))))

  ; vec-length : Vec -> Int
  (define (vec-length vector)
    (cases vec vector
      (a-vector (head length) length)))

  ; vec-ref : Vec x Int -> ExpVal
  (define (vec-ref vector index)
    (cases vec vector
      (a-vector (head length)
        (if (and (>= index 0) (< index length))
            (deref (+ head index))
            (eopl:error 'vec-ref "Index out of bounds")))))

  ; vec-copy : Vec -> Vec
  (define (vec-copy vector)
    (cases vec vector
      (a-vector (head length)
        (let ((copy (vec-zeros length)))
          (let loop ((i 0))
            (if (= i length) copy
                (begin (vec-set! copy i (deref (+ head i))) (loop (+ i 1)))))))))

  ; vec-swap! : Vec x Int x Int -> Void
  (define (vec-swap! vector index1 index2)
    (cases vec vector
      (a-vector (head length)
        (if (and (>= index1 0) (< index1 length) (>= index2 0) (< index2 length))
            (let ((temp (deref (+ head index1))))
              (setref! (+ head index1) (deref (+ head index2)))
              (setref! (+ head index2) temp))
            (eopl:error 'vec-swap! "Index out of bounds")))))

  ; #####################################################
  ; Part B.
  ; #####################################################

  ; queue-new : Int -> Queue
  (define (queue-new n)
    (a-queue (vec-new n 0)
             (newref 0)
             (newref -1)
             (newref 0)))

  ; queue-empty? : Queue -> Bool
  (define (queue-empty? q)
    (cases queue q
      (a-queue (data front rear size)
        (= (deref size) 0))))

  ; queue-full? : Queue -> Bool
  (define (queue-full? q)
    (cases queue q
      (a-queue (data front rear size)
        (= (deref size) (vec-length data)))))

  ; set-front! : Queue x Int -> Void
  (define (set-front! q value)
    (cases queue q
      (a-queue (data front rear size)
        (if (and (>= value 0) (< value (vec-length data)))
            (setref! front value)
            (eopl:error 'set-front! "Index out of bounds")))))

  ; set-rear! : Queue x Int -> Void
  (define (set-rear! q value)
    (cases queue q
      (a-queue (data front rear size)
        (if (and (>= value 0) (< value (vec-length data)))
            (setref! rear value)
            (eopl:error 'set-rear! "Index out of bounds")))))

  ; queue-enqueue! : Queue x ExpVal -> Void
  (define (queue-enqueue! q value)
    (cases queue q
      (a-queue (data front rear size)
        (if (queue-full? q)
            (eopl:error 'queue-enqueue! "Queue is full")
            (begin (set-rear! q (modulo (+ (deref rear) 1) (vec-length data)))
                   (vec-set! data (deref rear) value)
                   (setref! size (+ (deref size) 1)))))))

  ; queue-dequeue! : Queue -> ExpVal
  (define (queue-dequeue! q)
    (cases queue q
      (a-queue (data front rear size)
        (if (queue-empty? q)
            (num-val -1)
            (begin (let ((value (vec-ref data (deref front))))
                     (set-front! q (modulo (+ (deref front) 1) (vec-length data)))
                     (setref! size (- (deref size) 1))
                     value))))))

  ; queue-size : Queue -> Int
  (define (queue-size q)
    (cases queue q
      (a-queue (data front rear size)
        (deref size))))

  ; queue-peek : Queue -> ExpVal
  (define (queue-peek q)
    (cases queue q
      (a-queue (data front rear size)
        (if (queue-empty? q)
            (num-val -1)
            (vec-ref data (deref front))))))

  ; queue-print : Queue -> Void
  (define (queue-print q)
    (cases queue q
      (a-queue (data front rear size)
        (let loop ((i 0) (index (deref front)))
          (if (= i (deref size))
              (newline)
              (begin (display (vec-ref data i))
                     (display " ")
                     (loop (+ i 1) (modulo (+ index 1) (vec-length data)))))))))


  ; #####################################################
  ; Part C.
  ; #####################################################

  ; vec-mult : Vec x Vec -> Vec
  (define (vec-mult vector1 vector2)
    (cases vec vector1
      (a-vector (head1 length1)
        (cases vec vector2
          (a-vector (head2 length2)
            (if (= length1 length2)
                (let ((result (vec-new length1 0)))
                  (let loop ((i 0))
                    (if (= i length1) result
                        (begin (vec-set! result i
                                  (num-mult (deref (+ head1 i)) (deref (+ head2 i)))) 
                                (loop (+ i 1))))))
                (eopl:error 'vec-mult "Vectors must be of same length")))))))

  ; num-mult : NumVal x NumVal -> NumVal
  (define (num-mult num1 num2)
    (let ((n1 (expval->num num1))
          (n2 (expval->num num2)))
      (num-val (* n1 n2))))

)
