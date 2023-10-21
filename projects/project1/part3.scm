; PS 3.2.2
(define (count-true pred lower upper)
  (if (> lower upper) 0 
      (+ (if (pred lower) 1 0) (count-true pred (+ lower 1) upper))))

; PS 3.2.3
(define (count-true pred lower upper)
  (define (counter i) 
    (if (> i upper) 0 (+ (if (pred i) 1 0) (counter (+ i 1)))))
  (counter lower))

; PS 3.2.4
(define (accumulate-interval op init lower upper)
  (if (> lower upper) init
      (op lower (accumulate-interval op init (+ lower 1) upper))))

; PS 3.2.6
(define (repeatedly-apply p n)
  (if (= n 1) p (compose p (repeatedly-apply p (- n 1)))))

; PS 3.2.7
(define curry (lambda (p) (lambda (a) (lambda (b) (p a b)))))