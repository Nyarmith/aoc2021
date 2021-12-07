;; libraries included with CHICKEN.
(import (chicken io)
        (chicken string))

(define (input-list)
  (let ((line (read-line)))
      (if (not (eof-object? line))
       (cons (string-split line " ") (input-list))
       '()
    )))

(define (get-submarine-pos cur l) ; in cur - position 0 is forward, 1 is aim, 2 is depth
  (if (null? l)
    cur
    (begin
      (let ((x (vector-ref cur 0))
          (y (vector-ref cur 1))
          (z (vector-ref cur 2))
          (cmd (car l)))
      (let ((dir (car cmd))
           (shift (string->number (cadr cmd))))
        (if (string=? dir "forward")
          (begin
            (vector-set! cur 0 (+ x shift))
            (vector-set! cur 2 (+ z (* y shift))))
          (if (string=? dir "up")
            (vector-set! cur 1 (- y shift))
            (vector-set! cur 1 (+ y shift))
          ))))
        (get-submarine-pos cur (cdr l)))))

(define (main args)
  (let ((final-pos (get-submarine-pos (vector 0 0 0) (input-list))))
    (display (* (vector-ref final-pos 0) (vector-ref final-pos 2)))))
