#lang pl

(define x '("Yo" 1 2))
; this function get a list any type and give you the max element
(: get_max : (Listof Any) -> Number)
(define (get_max x)
  (define (nulling_nonNumbers a)
    (cond
      [(number? a) a]
      [else null]
      ))
  (apply max (filter (lambda (e) (number? e)) (map nulling_nonNumbers x)))
)
(: get_min : (Listof Any) -> Number)
(define (get_min x)
  (define (nulling_nonNumbers a)
    (cond
      [(number? a) a]
      [else null]
      ))
  (apply min (filter (lambda (e) (number? e)) (map nulling_nonNumbers x)))
)

(get_min '(1 2 '() 3))
;(define lst '(1 2 '() 3))
;(filter (lambda (e) (number? e)) lst)

;(map nulling_nonNumbers x)
;(apply max (map filter x))

;(apply max '(1 2 '() 3)) failed for needing numbers only


;(apply max '(10 8))

;(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3)))) => '((8 10) ()))



(map (lambda (x) 88) '((1 2) (3 4)))




(: min&max-lists : (Listof Any) -> (Listof Any))
(define (min&max-lists lst)
  (: min_max_list : (Listof Any) -> (Listof Any))
  (define (min_max_list l)
    (cond
      [(null? l) l]
      [else  (list (get_min l) (get_max l))])
    ) 
  (cond
    [(list? lst) (map min_max_list lst)]
    [else (map min_max_list (listof lst))])
  )

; Tests

(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3)))) => '((8 10) ()))
(test (min&max-lists '(any "Benny" 10 OP 8)) => '((8 10)))
