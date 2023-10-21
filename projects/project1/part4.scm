; PS 4.1.2
(define (contains? lst el)
  (cond ((equal? lst nil) #f)
        ((equal? (car lst) el) #t)
        ((> (length (cdr lst)) 0) (contains? (cdr lst) el))
        (else #f)))

; PS 4.1.4
(define (every-other lst)
  (if (< (length lst) 2) lst
      (cons (car lst) (every-other (cdr (cdr lst))))))

; PS 4.1.6
(define (intersection list1 list2)
  (define ls '())
  (map (lambda (x) (if (contains? list2 x) (set! ls (append ls (list x))) #f)) list1)
  ls)

; PS 4.2.1
(define (make-increasing-list n start)
  (define (iter next ls)
    (if (> (+ start n) next) (iter (+ next 1) (append ls (list next))) ls))
  (iter start '()))

; PS 4.2.2
(define (tree-ref index tree)
  (cond ((null? index) tree)
        (else (tree-ref (cdr index) (list-ref tree (car index))))))

; PS 4.3.1.1
(define (generate-interval a b)
  (define (iter next range)
    (if (< next b) (iter (+ next 1) (append range (list next))) range))
  (iter a '()))

; PS 4.3.1.2
(define (remove-divisible lst value)
  (define (div-helper x) (> (remainder x value) 0))
  (filter div-helper lst))

; PS 4.3.1.3
(define (sieve lst)
  (define (iter result wip)
    (if (null? wip) result
        (iter (append result (list (car wip))) 
              (remove-divisible (cdr wip) (car wip)))))
  (iter '() lst))

; PS 4.3.2.1
(define (transpose mat)
  (define (pair-row m)
    (if (null? m) (map (lambda (x) '()) (car mat))
      (map cons (car m) (pair-row (cdr m)))))
  (if (null? mat) '() (pair-row mat)))

; PS 4.3.2.2
(define (make-column-increasing-matrix rows cols start) 
    (transpose (make-increasing-matrix cols rows start)))

; PS 4.3.4.1
(define (lengths lst) (map string-length lst))

; PS 4.3.4.2
(define (total-length lst) (fold-right + 0 (map string-length lst)))

; PS 4.3.4.3
(define (string-em-up lst)
  (fold-right (lambda (e a) (+ a (if (string? e) (string-length e) 0))) 0 lst))

; PS 4.4.1
(define (map-tree op tree)
  (define (nest el)
    (cond ((number? el) (op el))
          ((not (null? el)) (map-tree op el))
          (else '())))
  (if (null? tree) tree (map nest tree)))

; PS 4.4.2
(define (fold-right-tree op id tree)
  (define (folder a res) (if (leaf? a) (op res a) (fold res a)))
  (define (fold init el) (fold-right folder init el))
  (if (null? tree) id (fold id tree)))