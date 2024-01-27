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
 
|#