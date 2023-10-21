(define second-order (lambda (x) (- (+ (* 3 (* x x)) (* 14 x)) 5) ))

(define quadratic-root
  (lambda (a b c) (/ (+ (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)) ))

(define pythagoras (lambda (a b) (sqrt (+ (* a a) (* b b))) ))

(define bigger2 (lambda (a b) (if (> a b) a b) ))

(define bigger3 (lambda (a b c)
                  (if (> a b)
                      (if (> a c) a c)
                      (if (> b c) b c))))