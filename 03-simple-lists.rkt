#lang racket
(provide (all-defined-out))
(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple list functions

;; Follow this template for functions on lists of numbers where appropriate.
;; [Listof Number] ... -> ...
#;
(define (lon-template ls ...)
  (match ls
    ['() ...]
    [(cons n ls) (... n (lon-template ls ...) ...)]))

;; [Listof Number] -> Natural
;; Compute the length of given list of numbers
(define (length-lon ls)
  (match ls

    [`() 0]
    [(cons h t) (+ 1 (length-lon t))]

    )
  )

(module+ test
  (check-equal? (length-lon '()) 0)
  (check-equal? (length-lon '(1)) 1)
  (check-equal? (length-lon '(2)) 1)
  (check-equal? (length-lon '(1 2)) 2))

;; [Listof Number] -> Number
;; Compute the sum of given list of numbers
(define (sum ls)
  (match ls
    [`() 0]
    [(cons h t) (+ h (sum t))]

    )
  )

(module+ test
  (check-equal? (sum '()) 0)
  (check-equal? (sum '(1)) 1)
  (check-equal? (sum '(2)) 2)
  (check-equal? (sum '(1 2)) 3))

;; [Listof Number] [Listof Number] -> [Listof Number]
;; Compute the pairwise sum of given list of numbers
;; ASSUME: lists have equal length
(define (zip-add ls1 ls2)

  (match ls1

    [`() `()]
    [(cons h1 t1) (match ls2

                    [`() `()]
                    [ (cons h2 t2) (cons (+ h1 h2) (zip-add t1 t2) )]

                    )]
    )
  )

(module+ test
  (check-equal? (zip-add '() '()) '())
  (check-equal? (zip-add '(1) '(2)) '(3))
  (check-equal? (zip-add '(1 3) '(2 4)) '(3 7))

  ;;Student Made Tests
  (check-equal? (zip-add '(1 3 2 4) '(2 4 5 6)) '(3 7 7 10))
  (check-equal? (zip-add '(1 2 3 4 5) '(1 2 3 4 5)) '(2 4 6 8 10))
  )

;; [Listof Number] [Listof Number] -> [Listof [List Number Number]]
;; Compute the pairwise list of given list of numbers
;; ASSUME: lists have equal length
(define (zip-lon ls1 ls2)

  (match ls1
    [`() `()]
    [ (cons h1 t1) (match ls2

                     [`() `()]
                     [(cons h2 t2) (cons (cons h1 (cons h2 `())) (zip-lon t1 t2))]
                     
                     )
                   ]
    
    )
  )

(module+ test
  (check-equal? (zip-lon '() '()) '())
  (check-equal? (zip-lon '(1) '(2)) '((1 2)))
  (check-equal? (zip-lon '(1 3) '(2 4)) '((1 2) (3 4))))

;; [Pairof Real [Listof Real]] -> Real
;; Compute a minimum element of non-empty list of numbers
(define (min-lon xs)

  (match xs
    [(cons h`()) h]
    [ (cons h t) (let ([x (min-lon t)]) (if (< h x) h x))]
    )

  
  )

(module+ test
  (check-equal? (min-lon '(1)) 1)
  (check-equal? (min-lon '(1 2)) 1)
  (check-equal? (min-lon '(2 1)) 1)
  (check-equal? (min-lon '(2 3 1)) 1)
  (check-equal? (min-lon '(1 25 9 63 8 7 5 6 2 5 88 9  121 0 23 655 47 8 96 3 2 14 8 5 7)) 0)

  )

;; [Listof Real] -> [Listof Real]
;; Sort list into descending order
;; HINT: do insertion sort by writing and using the helper below
(define (sort-desc xs)

  (match xs
    [`() `()]
    [(cons h t) (insert-desc h (sort-desc t))]
    )
  )

(module+ test
  (check-equal? (sort-desc '()) '())
  (check-equal? (sort-desc '(1)) '(1))
  (check-equal? (sort-desc '(1 2)) '(2 1))
  (check-equal? (sort-desc '(2 1)) '(2 1))
  (check-equal? (sort-desc '(2 3 1)) '(3 2 1)))

;; Real [Listof Real] -> [Listof Real]
;; Insert number into sorted list
;; ASSUME: given list is sorted in descending order
(define (insert-desc n xs)
  (match xs
    [`() (cons n `())]
    [ (cons h t) (if (< h n) (cons n xs) (cons h (insert-desc n t)))]
    )
  )

(module+ test
  (check-equal? (insert-desc 5 '()) '(5))
  (check-equal? (insert-desc 5 '(7)) '(7 5))
  (check-equal? (insert-desc 5 '(3)) '(5 3)))
