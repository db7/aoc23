#lang racket

(define-syntax-rule (assert c)
  (when (not c)
    (display "Assertion `")
    (display #'c)
    (displayln "' failed.")
    (raise 'AssertionError)))

(struct pick (r g b) #:transparent)

;; split a line into a list of picks
(define (get-pick p)
  (define r (regexp-match #rx"([0-9]+) red" p))
  (define g (regexp-match #rx"([0-9]+) green" p))
  (define b (regexp-match #rx"([0-9]+) blue" p))
  (define rgb (map (lambda (v) (if (not v) 0 (string->number (last v)))) (list r g b)))
  (apply hash (flatten (map list (list 'r 'g 'b) rgb))))
(define (get-picks line)
  (for/list ([i (string-split line ";")])
    (get-pick i)))

(define (get-game-number game-str)
  (string->number (last (string-split game-str " "))))

;; split ":" from line and return pair (number (list picks))
(define (get-game line)
  (let ([p-g (string-split line ":")])
    (assert (equal? 2 (length p-g)))
    (cons (get-game-number (first p-g)) (get-picks (last p-g)))))

(define (min-col picks col)
  (define (iter picks m)
    (if (empty? picks) m (let ([cur (hash-ref (car picks) col)]) (iter (cdr picks) (max cur m)))))
  (iter picks 0))

;; parse and filter only the possible games
(define (game-power line)
  (let* ([g (get-game line)] [num (car g)] [picks (cdr g)])
    (define vals (list (min-col picks 'r) (min-col picks 'g) (min-col picks 'b)))
    (printf "game ~a: ~a\n" num vals)
    (apply * vals)))

;; get the id (car) of each possible game and sum them up
(define (solve lines)
  (apply + (map game-power lines)))

(define in-file (make-parameter #f))
(define parser (command-line #:once-each [("-i") FILE "input file" (in-file FILE)] #:args () (void)))

(when (in-file)
  (printf "input file ~a\n" (in-file))
  (solve (file->lines (in-file))))
