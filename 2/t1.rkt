#lang racket

(define-syntax-rule (assert c)
  (when (not c)
    (display "Assertion `")
    (display #'c)
    (displayln "' failed.")
    (raise 'AssertionError)))

;; setup
(define max-cubes (hash 'r 12 'g 13 'b 14))

(struct pick (r g b) #:transparent)

;; split a line into a list of picks
(define (get-pick p)
  (define r (regexp-match #rx"([0-9]+) red" p))
  (define g (regexp-match #rx"([0-9]+) green" p))
  (define b (regexp-match #rx"([0-9]+) blue" p))
  (define rgb (map (lambda (v) (if (not v) 0 (string->number (last v)))) (list r g b)))
  (apply pick rgb))
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

;; determine if picks are possible
(define (is-possible? g)
  (define r
    (for/and ([round (cdr g)])
      (and (<= (pick-r round) (hash-ref max-cubes 'r))
           (<= (pick-g round) (hash-ref max-cubes 'g))
           (<= (pick-b round) (hash-ref max-cubes 'b)))))
  (printf "game ~a: ~a = ~a\n" (first g) r (cdr g))
  r)

;; parse and filter only the possible games
(define (possible-games lines)
  (filter is-possible? (map get-game lines)))

;; get the id (car) of each possible game and sum them up
(define (solve lines)
  (apply + (map car (possible-games lines))))

(define in-file (make-parameter #f))
(define parser (command-line #:once-each [("-i") FILE "input file" (in-file FILE)] #:args () (void)))

(when (in-file)
  (printf "input file ~a\n" (in-file))
  (solve (file->lines (in-file))))
