;; libraries included with CHICKEN.
(import (chicken io)
        (chicken bitwise))

(define (input-list)
  (let ((line (read-line)))
    (if (not (eof-object? line))
       (cons (map (lambda (c) (- (char->integer c) (char->integer #\0))) (string->list line)) (input-list))
       '()
    )))

(define (get-bit-counts l)
  (let ((bitlen (length (car l))))
    (foldr (lambda (a b) (map + a b)) (car l) (cdr l))))
    
(define (binlist->number l)
  (if (null? l) 0
   (+ (* (car l) (expt 2 (- (length l) 1))) (binlist->number (cdr l)))))

(define (main args)
  (let ((biglist (input-list)))
  (let ((bits (length (car biglist)))
        (gamma-list (map 
          (lambda (x) (if (> x (/ (length biglist) 2)) 1 0))
          (get-bit-counts biglist))))
  (let ((gamma (binlist->number gamma-list)))
  (let ((epsilon (bitwise-and (bitwise-not gamma) (- (expt 2 (length gamma-list)) 1))))
    (display (* gamma epsilon)))))))
  
