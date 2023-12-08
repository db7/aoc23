#lang racket

(require rackunit
         "../utils.rkt")

(struct card (n wn hn) #:transparent)

(define (parse-card line)
  (define pl (regexp-match #rx"Card *([0-9]+):([0-9 ]*)[|]([0-9 ]*)$" line))
  (assert (= 4 (length pl)))
  (define n (string->number (second pl)))
  (define wn (map string->number (string-split (third pl))))
  (define hn (map string->number (string-split (fourth pl))))
  (card n wn hn))

(check-equal? ;
 (parse-card "Card 1: 41 48 83 | 83 86  6 31")
 (card 1 '(41 48 83) '(83 86 6 31)))

(define (score-card c)
  (define (iter wn hn score)
    (cond
      [(empty? hn) score]
      [(and (member (car hn) wn) ;
            (= 0 score))
       (iter wn (rest hn) 1)]
      [(member (car hn) wn) (iter wn (rest hn) (* score 2))]
      [else (iter wn (rest hn) score)]))
  (iter (card-wn c) (card-hn c) 0))

(check-equal? 1 (score-card (card 1 '(1 2) '(2 3 4))))
(check-equal? 2 (score-card (card 1 '(1 2 3) '(2 3 4))))
(check-equal? 4 (score-card (card 1 '(2 3 4) '(2 3 4))))
(check-equal? 8 (score-card (card 1 '(2 3 4 8) '(2 3 4 8))))

(define (solve lines)
  (define cards (map parse-card lines))
  (foldr + 0 (map score-card cards)))

(when (in-file)
  (printf "input file ~a\n" (in-file))
  (solve (file->lines (in-file))))
