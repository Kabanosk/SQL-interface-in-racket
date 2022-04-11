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

(define countries1
  (table
   (list 
         (column-info 'country 'string) 
         (column-info 'population 'number)
         (column-info 'city 'string)
)
   (list (list "Poland" 38 "Wrocław")
         (list "Germany" 83  "Munich")
         (list "France" 67 "Paris" )
         (list "Spain" 47 "Barcelona" ))))
; display

(define (line n)
    (display "\n")
    (define (pom n)
        (display "-----\t")
        (if (> n 0) (pom (- n 1)) (display "\n"))
    )
    (pom (- n 1)) )

(define (table-display tab) 
    (define x (length (table-schema tab)))
    (define (display-schema elem row)
        (display (column-info-name elem))
        (display "\t")
        (if (null? row) 
            (line x)
            (display-schema (car row) (cdr row))))

    (define (print-row elem row)
        (display elem)
        (display "\t")
        (if (null? row) null (print-row (car row) (cdr row))))

    (define (display-rows row rows)
        (print-row (car row) (cdr row))
        
        (cond [(null? rows) 
                (line x)]
            [else 
                (display "\n")
                (display-rows (car rows) (cdr rows))]))

    (display-schema (car (table-schema tab)) (cdr (table-schema tab)))
    (if (null? (table-rows tab)) 
        (display "\n") 
        (display-rows (car (table-rows tab)) (cdr (table-rows tab)))))

(define tab5 (table-natural-join cities countries1))
(define tab1 (table-rename 'capital 'country132 cities))

(define tab6 (table-sort '(country area) cities))
(define tab4 (table-select  (not-f (lt-f 'capital #f)) cities))
(table-display tab6)
(display "\n\n")
;;; (table-display  (table-cross-join cities countries1))
(define tab (table-insert (list "Rzeszow" "Poland" 129 #f) cities))
(define tab2 (table-project '(city country capital) tab))
(define tab3 (table-cross-join cities countries))