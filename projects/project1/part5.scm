; PS 5.1.2
(define (empty? lst) (= (length lst) 0))
(define memq 
  (lambda (word text)
    (cond ((empty? text) #f)
          ((equal? (car text) word) text)
          (else (memq word (cdr text))))))

; PS 5.1.3
(define *english-to-french*
        '((cat chat) (cake gateau) (present cadeau) (I je) (eat mange) (the le)))

(define (lookup word dict)
  (define len (length dict))
  (define (get-pair i) (list-ref dict i))
  (define (keyeq? i key) (eq? (car (get-pair i)) key))
  (define (getval i) (cadr (get-pair i)))
  (define (look-for i) 
    (cond ((>= i len) #f)
          ((keyeq? i word) (getval i))
          (else (look-for (+ i 1)))))
  (look-for 0))

(define (translate sent dict)
  (map (lambda (word) (lookup word dict)) sent))

; PS 5.1.4
(define *document* '(A tutor who tooted the flute tried to tutor two tooters
                       to toot Said the two to the tutor is it harder to toot 
                       or to tutor two tooters to toot))

;;; Given a document, return a histogram
;;; List < symbol > -> histogram
(define make-histogram
  (lambda (document)
    (add-words-to-histogram document (make-empty-histogram))))

;;; Make a new histogram with no words in it
;;; null -> histogram
(define make-empty-histogram
  (lambda () '()))

;;; Given a word and a histogram, return #t if the word is in the histogram,
;;; otherwise return #f
;;; symbol,histogram->boolean
(define in-histogram? 
  (lambda (word histogram)
    (and (pair? histogram)
	 (or (eq? word (first (car histogram)))
	     (in-histogram? word (cdr histogram))))))

;;; Add a new word to a histogram with a count of 1
;;; word,histogram->histogram
(define add-new-word-to-histogram
  (lambda (word histogram)
    (cons (list word 1) histogram)))

(define (increment-word-count-in-histogram word histogram)
  (define (iter seq rest)
    (cond ((null? seq) #f)
          ((eq? word (caar seq))
           (append rest (list (list word (+ (cadar seq) 1))) (cdr seq)))
          (else (iter (cdr seq) (append rest (list (car seq)))))))
  (iter histogram '()))

(define (add-words-to-histogram words histogram)
  (define (handle-words words-list wip)
    (if (null? words-list) wip
        (let ((current-word (car words-list)))
             (handle-words (cdr words-list)
                  (if (in-histogram? current-word wip)
                      (increment-word-count-in-histogram current-word wip)
                      (add-new-word-to-histogram current-word wip))))))
  (handle-words words histogram))

(define (times-occuring histogram word)
  (cond ((not (pair? histogram)) 0)
        ((eq? word (caar histogram)) (cadar histogram))
        (else (times-occuring (cdr histogram) word))))

; PS 5.2.1
(define (make-complex-from-rect rl im) (list rl im))
(define (make-complex-from-polar mg an)
  (list (* mg (cos an))
        (* mg (sin an))))
(define (real cx) (car cx))
(define (imag cx) (cadr cx))
(define (mag cx) (sqrt (+ (square (real cx)) (square (imag cx)))))
(define (angle cx) (atan (imag cx) (real cx)))

; PS 5.2.5


