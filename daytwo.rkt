#lang racket

(define convert-string-number
  (map (lambda (line)
         (map string->number (string-split line)))
       (file->lines "input.txt")))

(define (Finc-dec alist pos)
  (cond
    [(empty? (list-ref alist pos)) '()]
    [(not (equal? (length (list-ref alist pos)) 
                  (length (remove-duplicates (list-ref alist pos))))) #f]
    [(equal? (list-ref alist pos) (sort (list-ref alist pos) >))]
    [(equal? (list-ref alist pos) (sort (list-ref alist pos) <))]
    (else #f
          )))

(define firstfilter
  (map cons convert-string-number (map (lambda (x) (Finc-dec convert-string-number x)) (range (length convert-string-number)))))


(define (Fbuildpairs l)
  (cond
    [(or (null? l) (null? (cdr l))) '()]
    (else
     (cons (list (car l) (cadr l))
       (Fbuildpairs (cdr l))))))

(define (Fcountdiff l)
  (abs (- (car l) (cadr l))))

(define (removef lst)
  (empty? (filter (lambda (x) (> x 3)) lst)))

(count (lambda (x) (equal? x #t)) (map removef (map (lambda (sublist) (map Fcountdiff (Fbuildpairs sublist))) (map (lambda (x) (filter number? x))
     (filter (lambda (sublist) (not (member #f sublist)))
             (map (lambda (x) (flatten x)) firstfilter))))))













