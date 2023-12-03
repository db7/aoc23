#lang racket

(require rackunit
         "../utils.rkt")

(define (find-symbols line)
  (map car (regexp-match-positions* #rx"[#%&*+/=@$-]" line)))

(check-equal? (find-symbols "...%...*..") '(3 7))
(check-equal? (find-symbols "...%.12*..") '(3 7))

(define (find-numbers line)
  (regexp-match-positions* #rx"[0-9]+" line))

(check-equal? (find-numbers "..10.+.321..") '((2 . 4) (7 . 10)))

;; nums are position ranges and syms are start positions
(define (part-numbers nums syms)
  (define (neighboring n)
    (for/or ([s syms])
      (and (>= s (- (car n) 1)) (<= s (cdr n)))))
  (filter neighboring nums))

(check-equal? ;
 (part-numbers (find-numbers "..10..20..30..40") ;
               (find-symbols ".+.....+....+...")) ;
 '((2 . 4) (6 . 8) (10 . 12)))

(define (substrings str pairs)
  (for/list ([p pairs])
    (substring str (car p) (cdr p))))

(check-equal? (substrings ;
               "hello world"
               '((0 . 3) (4 . 7) (6 . 8)))
              '("hel" "o w" "wo"))

(define (find-cur-parts prev cur next)
  (define cur-nums (find-numbers cur))
  (define part-pos
    (append (part-numbers cur-nums (find-symbols prev))
            (part-numbers cur-nums (find-symbols cur))
            (part-numbers cur-nums (find-symbols next))))
  (map string->number (substrings cur part-pos)))

(check-equal? (find-cur-parts ;
               "..#....$.."
               "..10..3..2"
               ".....$....")
              '(10 3 3))

(define (find-parts-dups lines)
  (define (iter lines result)
    (if (<= (length lines) 2)
        result
        (let ([prev (first lines)] ;
              [cur (second lines)]
              [next (third lines)])
          (define res (find-cur-parts prev cur next))
          (iter (rest lines) (append result res)))))
  (iter (append '("") lines '("")) '()))

(check-equal? (find-parts-dups (list ".1#....$.." ;
                                     "..10..3+.2"
                                     ".....$.800"))
              '(1 10 3 3 3 800))

(define (find-parts lines) ;
  (remove-duplicates (find-parts-dups lines)))

(check-equal? (find-parts (list ".1#....$.." ;
                                "..10..3+.2"
                                ".....$.800"))
              '(1 10 3 800))

(define (solve lines)
  (foldr + 0 (find-parts-dups lines)))

(when (in-file)
  (printf "input file ~a\n" (in-file))
  (solve (file->lines (in-file))))
