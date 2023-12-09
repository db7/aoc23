#lang racket

(require rackunit
         "../utils.rkt")

(define (steps s)
  (if (or (empty? s) (empty? (rest s))) ;
      '()
      (cons (- (second s) (first s)) (steps (rest s)))))

(check-equal? (steps '(0 3 6 9 12 15)) '(3 3 3 3 3))

(define (all-zeros? s)
  (foldl (lambda (a b) (and a b)) #t (map zero? s)))
(check-true (all-zeros? '(0 0 0)))
(check-false (all-zeros? '(0 0 1)))

(define (fix-point s)
  (if (all-zeros? s) (list s) (cons s (fix-point (steps s)))))
(check-equal? (fix-point '(0 3 6 9)) '((0 3 6 9) (3 3 3) (0 0)))
(check-equal? (fix-point '(0 3 6)) '((0 3 6) (3 3) (0)))

(define (back-track fs)
  (if (empty? fs) '() (cons (first (first fs)) (back-track (rest fs)))))
(check-equal? (back-track '((1 2 3) (2 3 3 3) (0 2 4))) '(1 2 0))

(define (predict s)
  (foldr - 0 (back-track (fix-point s))))
(check-equal? (predict '(0 3 6 9 12 15)) -3)

(define (parse-line line)
  (map string->number (string-split line " ")))

(define (solve lines)
  (define predictions (map predict (map parse-line lines)))
  (printf "predictions: ~a\n" predictions)
  (foldl + 0 predictions))

(when (in-file)
  (printf "input file ~a\n" (in-file))
  (solve (file->lines (in-file))))
