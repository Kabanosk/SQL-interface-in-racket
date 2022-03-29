#lang racket
;(type "Rzeszow")
(provide (struct-out column-info)
         (struct-out table)
         (struct-out and-f)
         (struct-out or-f)
         (struct-out not-f)
         (struct-out eq-f)
         (struct-out eq2-f)
         (struct-out lt-f)
         ;table-insert
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

(define (get-types tab)
    (define x (table-schema tab))
    (define (pom xs x schema)
        (if (null? schema) 
            (append xs (list (column-info-type x)))
            (pom (append xs (list (column-info-type x))) (car schema) (cdr schema))))
    (pom '() (car x) (cdr x)))

(define (type-question elem)
    (cond [(equal? 'string elem) 
            (lambda (x) (string? x))]
        [(equal? 'number elem) 
            (lambda (x) (number? x))]
        [(equal? 'boolean elem) 
            (lambda (x) (boolean? x))]
        [(equal? 'symbol elem) 
            (lambda (x) (symbol? x))] ))

(define (check-columns row tab)
    (define types (get-types tab))
    (define (check-types x row type types)
        (define f (type-question type))
        (if (null? types) 
            (if (not (f x)) #f #t)
            (if (not (f x)) 
                #f
                (check-types (car row) (cdr row) (car types) (cdr types)))  
    ))
    (check-types (car row) (cdr row) (car types) (cdr types)))


(define (table-insert row tab) 
    (if (check-columns row tab)
        (table 
            (table-schema tab) 
            (append (table-rows tab) (list row)))
        tab))



; Projekcja
(define (line n)
    (display "\n")
    (define (pom n)
        (display "-----\t")
        (if (> n 0) (pom (- n 1)) (display "\n"))
    )
    (pom (- n 1)) )

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

    (define (display-rows row rows)
        (print-row (car row) (cdr row))
        
        (cond [(null? rows) 
                (line x)]
            [else 
                (display "\n")
                (display-rows (car rows) (cdr rows))]))

    (display-schema (car (table-schema tab)) (cdr (table-schema tab)))
    (display-rows (car (table-rows tab)) (cdr (table-rows tab))))


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

(define (table-rename name new-name tab)
    (define schema (table-schema tab))
    (define (pom acc x ys) 
        (cond [(null? ys) 
            (if (equal? name (column-info-name x))
                (append acc (list (column-info new-name (column-info-type x)))) 
                (append acc (list x)))]        
            [(equal? name (column-info-name x))
                (pom (append acc (list (column-info new-name (column-info-type x)))) 
                    (car ys) 
                    (cdr ys))]
            [else (pom (append acc (list x)) (car ys) (cdr ys))]))

    (define tmp (pom '() (car schema) (cdr schema)))
    (table 
        tmp
        (table-rows tab)))

; Złączenie kartezjańskie

(define (table-cross-join tab1 tab2) null)

; Złączenie

(define (table-natural-join tab1 tab2) null)

(define tab (table-insert (list "Rzeszow" "Poland" 129 #f) cities))
(table-project tab)
(define tab1 (table-rename 'capital 'ez tab))
(table-project tab1)
