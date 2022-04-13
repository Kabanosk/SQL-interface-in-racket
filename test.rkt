#lang racket/base

(require rackunit "solution.rkt")

(define cities
  (table
   (list (column-info 'city    'string)
         (column-info 'country 'string)
         (column-info 'area    'number)
         (column-info 'capital 'boolean)
         )
   (list (list "Wrocław" "Poland"  293 #f)
         (list "Warsaw"  "Poland"  517 #t)
         (list "Poznań"  "Poland"  262 #f)
         (list "Berlin"  "Germany" 892 #t)
         (list "Munich"  "Germany" 310 #f)
         (list "Paris"   "France"  105 #t)
         (list "Rennes"  "France"   50 #f))))

(define countries
  (table
   (list (column-info 'country 'string) 
         (column-info 'population 'number))
   (list (list "Poland" 38)
         (list "Germany" 83)
         (list "France" 67)
         (list "Spain" 47))))

(define countries1
  (table
    (list 
         (column-info 'country  'string) 
         (column-info 'population 'number)
         (column-info 'city 'string)) 
    (list (list "Poland" 38 "Wrocław")
         (list "Germany" 83  "Munich")
         (list "France" 67 "Paris" )
         (list "Spain" 47 "Barcelona" ))))

(define (tests-insert)
    (test-begin)
        (let ([tab (table-insert (list "Rzeszow" "Poland" 129 #f) cities)])
        (check-equal? (length (table-rows tab)) 8)))

(define (tests-project)
    (let ([tab (table-project '(city country capital) cities)])
        (check-equal? (length (table-schema tab)) 3))
    (let ([tab1 (table-project '(city) cities)])
        (check-equal? (length (table-schema tab1)) 1)))

(define (tests-sort)
    (let ([tab (table-sort '(country area) cities)])
        (check-equal? (car (table-rows tab)) (list "Rennes"  "France"   50 #f))
    )
    (let ([tab1 (table-sort '(country) countries)])
        (check-equal? (car (table-rows tab1)) (list "France" 67))))

(define (tests-select)
    (let ([tab (table-select (lt-f 'area 100) cities)])
        (check-equal? (car (table-rows tab)) (list "Rennes"  "France"   50 #f)))
    (let ([tab1 (table-select (and-f (not-f (lt-f 'area 270)) (eq-f 'capital #f)) cities)])
        (check-equal? (car (table-rows tab1)) (list "Wrocław" "Poland"  293 #f))))

(define (tests-rename)
    (let ([tab (table-rename 'city 'miasto cities)])
        (check-equal? (car (table-schema tab)) (column-info 'miasto 'string))
        (check-equal? (table-rows tab) (table-rows cities))))

(define (tests-cross-join)
    (let ([tab (table-cross-join cities countries)])
        (check-equal? (table-schema tab) (append (table-schema cities) (table-schema countries)))
        (check-equal? (car (table-rows tab)) (append (car (table-rows cities)) (car (table-rows countries))))))

(define (tests-natural-join) 
    (let ([tab (table-natural-join cities countries1)])
        (check-equal? (length (table-rows tab)) 3))
    (let ([tab1 (table-natural-join cities countries)])
        (check-equal? (car (table-rows tab1)) (list "Wrocław" "Poland"  293 #f 38))
    ))

(tests-insert)
(tests-project)
(tests-sort)
(tests-select)
(tests-rename)
(tests-cross-join)
(tests-natural-join)