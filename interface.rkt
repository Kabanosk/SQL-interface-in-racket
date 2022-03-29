#lang racket

(provide (struct-out column-info)
         (struct-out table)
         (struct-out and-f)
         (struct-out or-f)
         (struct-out not-f)
         (struct-out eq-f)
         (struct-out eq2-f)
         (struct-out lt-f)
         table-insert
         table-project
         table-sort
         table-select
         table-rename
         table-cross-join
         table-natural-join)

(define-struct column-info (name type) #:transparent)

(define-struct table (schema rows) #:transparent)

(define cities
  (table
   (list (column-info 'city    'string)
         (column-info 'country 'string)
         (column-info 'area    'number)
         (column-info 'capital 'boolean))
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

(define (empty-table columns) (table columns '()))

; Wstawianie

(define (table-insert row tab) null)

; Projekcja
(define (line n)
    (display "\n")
    (define (pom n)
        (display "-----\t")
        (if (> n 0) (pom (- n 1)) (display "\n"))
    )
    (pom (- n 1)) 
)

; TODO: Dodaj taby odnośnie typów (na stringi 2 taby(?))
(define (table-project tab) 
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

    (define (print-rows row rows)
        (print-row (car row) (cdr row))
        
        (cond [(null? rows) 
                (line x)]
            [else 
                (display "\n")
                (print-rows (car rows) (cdr rows))]))

    (display-schema (car (table-schema tab)) (cdr (table-schema tab)))
    (print-rows (car (table-rows tab)) (cdr (table-rows tab)))
)
(table-project cities)
; Sortowanie

(define (table-sort cols tab) null)

; Selekcja

(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))

(define (table-select form tab) null)

; Zmiana nazwy

(define (table-rename col ncol tab) null)

; Złączenie kartezjańskie

(define (table-cross-join tab1 tab2) null)

; Złączenie

(define (table-natural-join tab1 tab2) null)