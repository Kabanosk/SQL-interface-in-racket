#lang racket

(require "solution.rkt")

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


(define tab (table-insert (list "Rzeszow" "Poland" 129 #f) cities))
(define tab2 (table-project '(city country capital) tab))
(define tab3 (table-cross-join cities countries))
(define tab4 (table-select 
      (or-f (eq-f 'country "Poland")  (lt-f 'area 100))
      tab))
(define tab5 (table-natural-join cities cities))
(define tab6 (table-sort '(area) cities))

(table-display tab6)
(display "\n\n")
(define tab1 (table-rename 'capital 'is_capital tab))
(table-display  cities)