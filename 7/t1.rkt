#lang racket

(require rackunit
         "../utils.rkt")

(define (parse-hand line)
  ;(printf "parsing: ~a\n" line)
  (define card (string-split line " "))
  (assert (= 2 (length card)))
  (cons (first card) (string->number (second card))))

(define card-order (reverse (list #\A #\K #\Q #\J #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2)))

(define (cmp-card a b)
  (< (index-of card-order a) (index-of card-order b)))

(define (sum-hand h)
  (define cards (string->list h))
  ;(displayln cards)
  (foldl (lambda (card h) ;
           (hash-set h card (+ 1 (hash-ref h card 0))))
         (hash)
         cards))
(check-equal? (sum-hand "KKKTT") (hash #\K 3 #\T 2))

(define (hand-type h)
  (define sum (sum-hand h))
  (define uniq (hash-keys sum))
  (cond
    [(= 5 (length uniq)) 'high-card]
    [(= 4 (length uniq)) 'one-pair]
    [(and (= 3 (length uniq)) ;
          (= 2 (apply max (hash-values sum))))
     'two-pairs]
    [(and (= 3 (length uniq)) ;
          (= 3 (apply max (hash-values sum))))
     'three-kind]
    [(and (= 2 (length uniq)) ;
          (= 3 (apply max (hash-values sum))))
     'full-house]
    [(and (= 2 (length uniq)) ;
          (= 4 (apply max (hash-values sum))))
     'four-kind]
    [(= 1 (length uniq)) 'five-kind]
    [else (raise 'unexpected)]))

(check-equal? (hand-type "23456") 'high-card)
(check-equal? (hand-type "32T3K") 'one-pair)
(check-equal? (hand-type "KK677") 'two-pairs)
(check-equal? (hand-type "K7775") 'three-kind)
(check-equal? (hand-type "TTTKK") 'full-house)
(check-equal? (hand-type "TTTTK") 'four-kind)
(check-equal? (hand-type "TTTTT") 'five-kind)

(define types '(high-card one-pair two-pairs three-kind full-house four-kind five-kind))
(define type-score
  (make-hash (for/list ([t types] [s (in-naturals)])
               (cons t s))))

(define (cmp-cards a b)
  (let ([a-cards (string->list a)] [b-cards (string->list b)])
    (assert (= 5 (length a-cards)))
    (assert (= 5 (length b-cards)))
    (define (iter a b)
      (when (or (empty? a) (empty? b))
        (raise 'empty-lists))
      (cond
        [(cmp-card (car a) (car b)) #t]
        [(cmp-card (car b) (car a)) #f]
        [else (iter (rest a) (rest b))]))
    (iter a-cards b-cards)))
(check-true (cmp-cards "KK577" "KK677"))

(define (cmp-hands a b)
  (let ([a-score (hash-ref type-score (hand-type a))] ;
        [b-score (hash-ref type-score (hand-type b))])
    (cond
      [(> a-score b-score) #f]
      [(< a-score b-score) #t]
      [else (cmp-cards a b)])))

(check-true (cmp-hands "KKTTT" "AAAKK"))
(check-false (cmp-hands "33332" "2AAAA"))
(check-false (cmp-hands "77888" "77788"))
(check-true (cmp-hands "23332" "AAAAA"))
(check-true (cmp-hands "2A2A3" "TTT98"))

(define (sort-hands hands)
  (define sorted-hands
    (sort hands
          (lambda (a b) ;
            (cmp-hands (car a) (car b)))))
  (for/list ([hand sorted-hands] [i (in-naturals)])
    (list (+ i 1) hand)))

(define (bid-rank rhand)
  (let ([rank (car rhand)] ;
        [bid (cdr (second rhand))])
    (printf "~a: ~a | ~a\n" rhand rank bid)
    (* rank bid)))

(define (solve lines)
  (define sorted-hands (sort-hands (map parse-hand lines)))
  (displayln sorted-hands)
  (apply + (map bid-rank sorted-hands)))

(when (in-file)
  (printf "input file ~a\n" (in-file))
  (solve (file->lines (in-file))))
