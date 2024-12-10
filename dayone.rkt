#lang racket


(define records (map string-split (file->lines "input.txt")))

(define puzzle-data
  (map (lambda (sublist) (map string->number sublist)) records))

(define listA (map car puzzle-data))        
(define listB (flatten (map cdr puzzle-data)))

(define (Fbuild-pairs list1 list2)
  (map list (sort list1 <) (sort list2 <)))

(define (Fdistance-between-pairs list-of-pairs)
  (map abs (map (lambda (x) (- (car x) (car (cdr x)))) list-of-pairs)))

(define (Fsum-distance)
  (apply + (Fdistance-between-pairs
             (Fbuild-pairs listA listB))))

(Fsum-distance)
