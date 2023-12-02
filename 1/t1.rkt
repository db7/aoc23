#lang racket

(require rackunit)

(define (rev-string line) (list->string (reverse (string->list line))))
(define (first-digit line) (first (regexp-match #rx"[0-9]" line)))
(define (last-digit line) (first-digit (rev-string line)))

(define	(process-line line)
	  (let ([left (first-digit line)]
		[right (last-digit line)])
		(string->number (string-append left right))))

(define (solution lines)
    (apply + (map process-line lines)))

(check-equal?
	142
	(solution (list "1abc2"
		"pqr3stu8vwx"
		"a1b2c3d4e5f"
    		"treb7uchet")))


(define in-file (make-parameter #f))
(define parser
  (command-line
   #:once-each
   [("-i") FILE "input file" (in-file FILE)]
   #:args () (void)))

(when (in-file)
    (printf "input file ~a\n" (in-file))
    (solution (file->lines (in-file))))
