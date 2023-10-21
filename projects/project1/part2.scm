(define barometer-dir
  (lambda (p1 p2) (cond ((> p2 p1) "rising") ((= p2 p1) "steady") (else "falling")) ))

(define abs (lambda (n) (if (< n 0) (- n) n) )) 

(define (boolean-odd? x)
  (or (= x 1) (and (>= x 2) (boolean-odd? (- x 2)))))

(define slow-mul-recurse
  (lambda (a b) (if (= b 0) 0 (+ a (slow-mul-recurse a (- b 1)))) ))

(define (odd? x) (cond ((= x 1) #t) ((>= x 2) (odd? (- x 2))) (else #f)))

(define simple-log (lambda (n) (if (= n 1) 0 (+ 1 (simple-log (/ n 2)))) )) 