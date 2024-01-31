#lang pl
#|what were the main difficulties
  how you solved them
  how much time did you invest in solving it
  did you need to consult others.
  A solution without proper comments may be graded 0|#


;Question 1;
#|one of the difficult things is to make list inside list, but i used sub functions so each of them doing something else and break the problem
  into pices.
  another problem is to stope the function from count the last list, which makes it put empty list 
  at the end i used cond to check wheter the list is empty and if so i return the list, instead of list of empty list.
  the function took me 2 days to solve
|#
( : create-fixed-length-lists : (Listof Number) Number -> (Listof (Listof Number)))
(define (create-fixed-length-lists lst n)
 ; the function should create new sublist of first n elements
( : create_sublist : (Listof Number) Number -> (Listof Number))
(define (create_sublist og_lst acc)
     (cond
       [(zero? acc) '()]
       [(null? og_lst) '()]
       [(list? og_lst) (append (list (first og_lst)) (create_sublist (rest og_lst) (- acc 1)))]
       )
     )
; this function should help me get the rest of the list after n items
(: drop : (Listof Number) Number ->(Listof Number))
  (define (drop lst n)
    (cond
      [(<= n 0) lst]
      [(null? lst) lst]
      [else (drop (rest lst) (- n 1))])
    )
  (cond
    [(null? lst) lst]
    [(= n 0) '()]
    [else (append (list (create_sublist lst n)) (create-fixed-length-lists (drop lst n) n))])
  )
; Test Cases ;
(test (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 3) => '((1 2 3) (4 5 6) (7 8 9)))
(test (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 9) => '((1 2 3 4 5 6 7 8 9)))

; Test the function with an empty list
(test (create-fixed-length-lists '() 3) => '())

; Test the function with a list that is not a multiple of n
(test (create-fixed-length-lists '(1 2 3 4 5) 3) => '((1 2 3) (4 5)))

; Test the function with n equal to 1
(test (create-fixed-length-lists '(1 2 3 4 5) 1) => '((1) (2) (3) (4) (5)))

; Test the function with n equal to the length of the list
(test (create-fixed-length-lists '(1 2 3 4 5) 5) => '((1 2 3 4 5)))

; Test the function with a list of length less than n
(test (create-fixed-length-lists '(1 2 3) 5) => '((1 2 3)))

; Test the function with n equal to 0
(test (create-fixed-length-lists '(1 2 3 4 5) 0) => '())




;Question 2a;
#|
 took me 2 days
|#
(: nested-list-depth : (Listof Any) -> Number)
(define (nested-list-depth lst)
  (cond
    [(null? lst) 0] ; empty list the depth is 0
    [(list? (first lst)) ; if first element is a list
     ;get the max between those
     (max (+ (nested-list-depth (first lst)) 1) ; add 1 to the depth of the list
          (nested-list-depth (rest lst))) ] ; get the depth of the rest of the list
    [else (max 1 (nested-list-depth (rest lst)))] ; if the first element is not a list, so we check again the rest of the list
    ) 
  )
; Tests ;
(test (nested-list-depth '(1 (2 3) ((4)) (5 (6)))) => 3) 
(test (nested-list-depth '(1 2 3)) => 1)
(test (nested-list-depth '()) => 0)
(test (nested-list-depth '(1 (2 (3 4)) (5 6))) => 3) ; Test of list with two nested list inside
(test (nested-list-depth '()) => 0) ; An empty list has depth 0
(test (nested-list-depth '(1)) => 1) ; A list with one element has depth 1
(test (nested-list-depth '(1 (2))) => 2) ; A list with a nested list has depth 2
(test (nested-list-depth '(1 (2 (3)))) => 3) ; A list with two levels of nested lists has depth 3
(test (nested-list-depth '(1 (2 (3 (4))))) => 4) ; A list with three levels of nested lists has depth 4
(test (nested-list-depth '(1 (2) (3 (4 (5))))) => 4) ; The depth is determined by the deepest nested list
(test (nested-list-depth '(1 (2 (3 (4))) (5 (6 (7))))) => 4) ; The depth is determined by the deepest nested list


;Question 2b;
#|
 the difficulty is to apply the the function on each of the sublist,
 second we need to check the max and min value of each sublist.
it took me almost 5 hours
|#
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
; this function get a list any type and give you the max element

(: get_min : (Listof Any) -> Number)
(define (get_min x)
  (define (nulling_nonNumbers a)
    (cond
      [(number? a) a]
      [else null]
      ))
  (apply min (filter (lambda (e) (number? e)) (map nulling_nonNumbers x)))
)

(: min&max-lists : (Listof Any) -> (Listof Any))
(define (min&max-lists lst)
  (: min_max_list : (Listof Any) (Listof Any) -> (Listof Any))
  (define (min_max_list l nl)
    (cond
      [(null? l) nl]
      [else  (min_max_list  (list (get_min l) (get_max l)))])
    )
  (min_max_list lst)
  )

; Tests

(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3)))) => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ())) => '((1 5) (1 7) ()))



;Question 3;
#|
   
|#

(define-type TaggedQueue
  [EmptyTQ]
  [Enqueue Symbol Any TaggedQueue])


(: search-queue : Symbol TaggedQueue -> Any)
(define (search-queue ID TQ)
  (cases TQ
    [(EmptyTQ) #f]
    [(Enqueue i val T)
     (cond
       [(eq? i ID) val]
       [else (search-queue ID T)])])
  )

( : dequeue-queue : TaggedQueue -> Any)
(define (dequeue-queue TQ)
  (cases TQ
    [(EmptyTQ) #f]
    [(Enqueue i val Q) Q])
  )

(test (EmptyTQ) => (EmptyTQ))
(test (Enqueue 'x 42 (EmptyTQ)) => (Enqueue 'x 42 (EmptyTQ)))
(test (search-queue 'x (Enqueue 'x 42 (EmptyTQ))) => 42)
(test (search-queue 'x (EmptyTQ)) => #f)
(test (search-queue 'x (Enqueue 'y 42 (Enqueue 'x 12 (EmptyTQ)))) => 12)
(test (dequeue-queue (Enqueue 'x 42 (EmptyTQ))) => (EmptyTQ))
(test (dequeue-queue (EmptyTQ)) => #f)
