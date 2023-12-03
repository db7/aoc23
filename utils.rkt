#lang racket

(define-syntax-rule (assert c)
  (when (not c)
    (display "assertion `")
    (display #'c)
    (displayln "' failed.")
    (raise 'ERROR)))

(define in-file (make-parameter #f))
(void (command-line #:once-each [("-i") FILE "input file" (in-file FILE)] #:args () (void)))

(provide assert
         in-file)
