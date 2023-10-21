;;;;;;;;;;;;;;;; Constant expressions ;;;;;;;;;;;;;;;;

(define constant-tag 'constant)

;;; number -> ConstantExp
(define make-constant (lambda (num)
    (list constant-tag num)))
  

;;; anytype -> boolean
(define constant-exp? (lambda (exp)
  (and (pair? exp)
       (eq? (car exp) constant-tag))))

;;; ConstantExp -> number
(define constant-val cadr)

;;; ConstantExp,ConstantExp->ConstantExp
(define constant-add (lambda (c1 c2)
    (make-constant (+ (constant-val c1)
		      (constant-val c2)))))

;;;;;;;;;;;;;;;;  Range expressions ;;;;;;;;;;;;;;;;

(define range-tag 'range)

;;; number, number -> RangeExp
(define make-range (lambda (lower upper)
  (list range-tag lower upper)))

;;; anytype -> boolean
(define range-exp? (lambda (exp)
  (and (pair? exp)
       (eq? (car exp) range-tag))))

;;; RangeExp -> number
(define range-min cadr)
(define range-max caddr)

;;; RangeExp,RangeExp -> RangeExp
(define range-add (lambda (r1 r2)
  (make-range (+ (range-min r1) (range-min r2))
	      (+ (range-max r1) (range-max r2)))))


;;;;;;;;;;;;;;;; Limited Precision expressions ;;;;;;;;;;;;;;;;

(define limited-tag 'limited)

;;; anytype -> boolean
(define limited-exp? (lambda (exp)
  (and (pair? exp)
       (eq? (car exp) limited-tag))))

;;; number,number -> LimitedExp
(define make-limited-precision (lambda (val err)
  (list limited-tag val err)))

;;;;;;;;;;;;;;;; Sum expression ;;;;;;;;;;;;;;;;

(define sum-tag '+)

;;; number,number -> SumExp
(define make-sum (lambda (addend augend)
  (list sum-tag addend augend)))

;;; anytype -> boolean
(define sum-exp? (lambda (exp)
  (and (pair? exp)
       (eq? (car exp) sum-tag))))

;;; SumExp -> number
(define sum-addend cadr)
(define sum-augend caddr)

;;;;;;;;;;;;;;;; Dealing with values in general ;;;;;;;;;;;;;;;;
;;;
;;; ValueExp = ConstantExp | RangeExp

;;; anytype -> boolean
(define value-exp? (lambda (v)
  (or (constant-exp? v) (range-exp? v))))

;;; ValueExp, ValueExp -> ValueExp
(define value-add (lambda (v1 v2)
  (cond ((and (constant-exp? v1) (constant-exp? v2))
	 (constant-add v1 v2))
	((and (value-exp? v1) (value-exp? v2))
	 (range-add (val2range v1) (val2range v2)))
	(else
	 (error "Unknown exp type")))))

;;; ValueExp -> RangeExp
(define val2range (lambda (v)
  (if (range-exp? v)
      v		    
      (make-range (constant-val v) (constant-val v)))))

;;;;;;;;;;;;;;;; Eval ;;;;;;;;;;;;;;;;

;;; ValueExp | LimitedExp | SumExp -> ValueExp | LimitedExp

(define eval-exp (lambda (exp)
  (cond ((value-exp? exp) exp)
	((limited-exp? exp) exp)
	((sum-exp? exp)
	 (value-add (eval-exp (sum-addend exp)) (eval-exp (sum-augend exp))))
	(else (error "Unknown expr type")))))

(define (make-canonical-limited-precision val err)
  (make-range (- val err) (+ val err)))

(define (make-canonical-constant c) (make-range c c))

(define (output-value exp)
  (define lp *output-ranges-as-limited-precision*)
  (if (or (not (list? exp)) (not (range-exp? exp))) 'error
      (let ((l (range-min exp)) (u (range-max exp)))
        (cond ((equal? l u) (list constant-tag l))
              (lp (list limited-tag (/ (+ u l) 2) (/ (- u l) 2)))
              (else exp)))))