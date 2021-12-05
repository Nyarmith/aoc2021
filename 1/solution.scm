;; libraries included with CHICKEN.
(import (chicken io))

(define (input-list)
  (let ((line (read-line)))
      (if (not (eof-object? line))
       (cons (string->number line) (input-list))
       '()
    )))

(define (print-input l)
  (if (not (null? l))
    (begin (display (car l))
     (newline)
     (print-input (cdr l)))))
     
(define (increase-binarize a b)
 (if (< a b) 1 0))

(define (map-pair f l)
  (if (or (null? l)
    (null? (cdr l))) '()
    (cons (f (car l) (cadr l)) (map-pair f (cdr l)))))

(define (total-ups l)
  (foldr + 0 (map-pair increase-binarize l)))

(define (main args)
  (display (total-ups (input-list))))
