#lang racket
(require racket/match)

(define pat "[0-9]|two|three|four|five|six|seven|eight|one|nine")
(define ahead-pat (string-append "(?=" pat ")"))

(define (split-digits_ line)
    (define pos-pair (regexp-match-positions* ahead-pat line))
    (define pos (map car pos-pair))
    (for/list ([p pos])
        (regexp-match pat line p)))

(define (split-digits line)
	(define r (map car (split-digits_ line)))
	(printf "--> ~a\n" r)
	r)

(define (digitize e)
	(match e
		["eight" "8"]
		["nine" "9"]
		["one" "1"]
		["two" "2"]
		["three" "3"]
		["four" "4"]
		["five" "5"]
		["six" "6"]
		["seven" "7"]
		[_ e]))
(define (first-digit line) (first (split-digits line)))
(define (last-digit line) (last (split-digits line)))

(define	(process-line line)
	  (let ([left (digitize (first-digit line))]
		[right (digitize (last-digit line))])
		(printf "~a ~a ~a ~a\n" left right (split-digits line) line)
		(string->number (string-append left right))))

(define (solve lines)
    (apply + (map process-line lines)))


(define in-file (make-parameter #f))
(define parser
  (command-line
   #:once-each
   [("-i") FILE "input file" (in-file FILE)]
   #:args () (void)))

(when (in-file)
    (printf "input file ~a\n" (in-file))
    (solve (file->lines (in-file))))
