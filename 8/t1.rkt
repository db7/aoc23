#lang racket

(require racket/hash
         rackunit
         "../utils.rkt")

(define (next-node edges dir)
  (if (equal? dir #\L) (car edges) (cdr edges)))

(define (travel-distance nodes left/right)
  (define (iter nodes node lr dist)
    (let* ([edges (hash-ref nodes node)] ;
           [dir (first lr)]
           [lr-rot (append (rest lr) (list dir))]
           [next (next-node edges dir)])
      (if (equal? next "ZZZ") dist (iter nodes next lr-rot (+ 1 dist)))))
  (iter nodes "AAA" left/right 1))

(define (parse-node line)
  (let ([node (last (regexp-match #rx"([A-Z]*) =" line))] ;
        [pair (rest (regexp-match #rx"([A-Z]*) *, *([A-Z]*)" line))])
    (hash node (cons (first pair) (last pair)))))
(check-equal? (parse-node "TSK = (CCB, MHN)") (hash "TSK" '("CCB" . "MHN")))

(define (parse-nodes lines)
  (foldr hash-union (hash) (map parse-node lines)))

(define (solve lines)
  ;; assuming text has form RL\n\nAAA=(BBB,CCC)
  (define left/right (string->list (first lines)))
  (define nodes (parse-nodes (rest (rest lines))))
  (travel-distance nodes left/right))

(when (in-file)
  (printf "input file ~a\n" (in-file))
  (solve (file->lines (in-file))))
