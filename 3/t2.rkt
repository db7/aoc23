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

;; keep track of how often the symbol was used
(define usage (make-hash))

(define (usage-add sln spos nln npos)
  (let ([key (cons sln spos)] ;
        [num (cons nln npos)])
    (hash-set*! usage
                key ;
                (if (hash-has-key? usage key) ;
                    (cons num (hash-ref usage key))
                    (list num))))
  #t)

;; nums are position ranges and syms are start positions
(define (part-numbers nums syms nln sln)
  (define (neighboring n)
    (for/or ([s syms])
      (if (and (>= s (- (car n) 1)) (<= s (cdr n))) ;
          (usage-add sln s nln n)
          #f)))
  (filter neighboring nums))

(check-equal? ;
 (part-numbers (find-numbers "..10..20..30..40") ;
               (find-symbols ".+.....+....+...")
               0
               0) ;
 '((2 . 4) (6 . 8) (10 . 12)))

(define (substrings str pairs)
  (for/list ([p pairs])
    (substring str (car p) (cdr p))))

(check-equal? (substrings ;
               "hello world"
               '((0 . 3) (4 . 7) (6 . 8)))
              '("hel" "o w" "wo"))

(define (find-cur-parts prev cur next cur-ln)
  (define cur-nums (find-numbers cur))
  (define part-pos
    (append (part-numbers cur-nums (find-symbols prev) cur-ln (- cur-ln 1))
            (part-numbers cur-nums (find-symbols cur) cur-ln cur-ln)
            (part-numbers cur-nums (find-symbols next) cur-ln (+ cur-ln 1))))
  (map string->number (substrings cur part-pos)))

(check-equal? (find-cur-parts ;
               "..#....$.."
               "..10..3..2"
               ".....$...."
               1)
              '(10 3 3))

(define (find-parts-dups lines)
  (define (iter lines result ln)
    (if (<= (length lines) 2)
        result
        (let ([prev (first lines)] ;
              [cur (second lines)]
              [next (third lines)])
          (define res (find-cur-parts prev cur next ln))
          (iter (rest lines) (append result res) (+ ln 1)))))
  (iter (append '("") lines '("")) '() 1))

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

(define (is-gear? h k lines)
  ;; index in lines -1
  (let* ([sln (car k)] ;
         [spos (cdr k)]
         [line (list-ref lines sln)]
         [sym (substring line spos (+ spos 1))])
    (and (equal? "*" sym) ;
         (= 2 (length (hash-ref h k))))))

(check-true (is-gear? ;
             (hash (cons 1 3) ;
                   (list (cons 1 (cons 2 3)) ;
                         (cons 1 (cons 4 5))))
             (cons 1 3)
             (list "" "..2*1..")))

(define (gear-ratio lines)
  (define (iter keys result)
    (cond
      [(empty? keys) result]
      [(is-gear? usage (car keys) lines)
       (iter (rest keys)
             (cons (apply *
                          (map (lambda (v) ;
                                 (let ([line (list-ref lines (car v))] ;
                                       [pos (cdr v)])
                                   (string->number ;
                                    (substring line (car pos) (cdr pos)))))
                               (hash-ref usage (car keys))))
                   result))]
      [else (iter (rest keys) result)]))
  (iter (hash-keys usage) '()))

(define (solve lines)
  (hash-clear! usage)
  (void (find-parts-dups lines))
  (foldr + 0 (gear-ratio (append '("") lines '("")))))

(when (in-file)
  (printf "input file ~a\n" (in-file))
  (solve (file->lines (in-file))))
