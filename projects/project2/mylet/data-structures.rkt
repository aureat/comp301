#lang eopl

;; data structures for let-lang.

(provide (all-defined-out))


;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

(define-datatype expval expval?
  (num-val
   (value number?))
  (rational-val
   (rational pair?))
  (bool-val
   (boolean boolean?))
  (list-of-nums-val
   (lst list?))
  
  ;; have to include this because of tests.rkt
  ;; but it's not part of MY-LET
  (str-val
    (str string?))
)


;;;;;;;;;;;;;;;;;;; extractors ;;;;;;;;;;;;;;;;;;;

;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

;; expval->list : ExpVal -> List
(define expval->list
  (lambda (v)
    (cases expval v
      (list-of-nums-val (lst) lst)
      (else (expval-extractor-error 'list-of-nums v)))))

;; expval->rational : ExpVal -> Int | Pair
(define expval->rational
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (rational-val (rat) rat)
      (else (expval-extractor-error 'rational v)))))

;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

;; expval->string : ExpVal -> String
(define expval->string
  (lambda (v)
    (cases expval v
      (str-val (str) str)
      (else (expval-extractor-error 'string v)))))

;; expval-extractor-error : Symbol x ExpVal -> Void
(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))


;;;;;;;;;;;;;;;;;;;;;;; list-val ;;;;;;;;;;;;;;;;;;;;;;;

;; list-val : List<Int> -> ExpVal(List<ExpVal(Num)>)
(define (list-val lst)
  (list-of-nums-val (map num-val lst)))


;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

(define empty-env-record
  (lambda () 
    '()))

(define extended-env-record
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define empty-env-record? null?)

(define environment?
  (lambda (x)
    (or (empty-env-record? x)
        (and (pair? x)
             (symbol? (car (car x)))
             (expval? (cadr (car x)))
             (environment? (cdr x))))))

(define extended-env-record->sym
  (lambda (r)
    (car (car r))))

(define extended-env-record->val
  (lambda (r)
    (cadr (car r))))

(define extended-env-record->old-env
  (lambda (r)
    (cdr r)))
