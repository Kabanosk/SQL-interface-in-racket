#lang racket

(provide (struct-out column-info)
         (struct-out table)
         (struct-out and-f)
         (struct-out or-f)
         (struct-out not-f)
         (struct-out eq-f)
         (struct-out eq2-f)
         (struct-out lt-f)
         get-specific-schema
         table-display
         table-insert
         table-project
         table-sort
         table-select
         table-rename
         table-cross-join
         table-natural-join)

(define-struct column-info (name type) #:transparent)

(define-struct table (schema rows) #:transparent)

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
            (f x)
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
        (error "You cannot add this row.")))
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

; projekcja

(define (get-specific-schema cols schema) ; wyciągamy kolumny cols ze schematu schema
    (define (pom x xs new-schema)
        (cond [(null? xs) 
                (if (is-in? (column-info-name x) cols)
                    (append new-schema (list x))
                    new-schema)]
            [(is-in? (column-info-name x) cols) 
                (pom (car xs) (cdr xs) (append new-schema (list x)))]
            [else 
                (pom (car xs) (cdr xs) new-schema)]))
    (pom (car schema) (cdr schema) '()))

(define (get-specific-rows cols tab)  ;lista wartości boolowskich
    (define idxs (indexes cols tab)) ;;(1011)
    (define (for row rows new-rows)
        (define (for2 idx el row acc)
            (if (null? row) 
                (append acc (if (equal? 1 (car idx)) (list el) null))
                (for2 (cdr idx) (car row) (cdr row) (append acc (if (equal? 1 (car idx)) (list el) null)))
            ))
        (if (null? rows)
            (append new-rows (list (for2 idxs (car row) (cdr row) '())))
            (for (car rows) (cdr rows)
                (append new-rows (list (for2 idxs (car row) (cdr row) '()))))
        ))
    (define rows (table-rows tab))
    (for (car rows) (cdr rows) '()))

(define (table-project cols tab) 
    (table 
        (get-specific-schema cols (table-schema tab)) 
        (get-specific-rows cols tab)))

;Sortowanie

(define (funkcja-wiekszosci type)
    ;;; (cond [(equal? type 'string) (lambda(x y) (string?> x y))]
    ;;;     [(equal? type 'number) (lambda(x y) (> x y))]
    ;;;     [(equal? type 'boolean) (lambda(x y) (boolean?> x y))]
    ;;;     [(equal? type 'symbol) 
    ;;;         (lambda(x y) (string?> (symbol->string x) (symbol->string y)))]
    ;;;     [else (error "Bad column type")])
    null
)

(define (find-minimum rows cols)
    ;;; (define (help row min_row rows col cols1 )
    ;;;     (define f (funkcja-wiekszosci col))
    ;;;     (cond [(= row min_row)]
    ;;;         [(< row min_row) (help (car rows) row (cdr rows) col cols1 )]
        
        
        
        
    ;;;     )

    ;;; )
    null
)

(define (table-sort cols tab) 
    (table 
        (table-schema tab) '() )
)

; Selekcja

(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))

(define (good-rows form row cols)
    (cond [(and-f? form) 
            (and (good-rows (and-f-l form) row cols) (good-rows (and-f-r form) row cols))]
        [(or-f? form) (or (good-rows (or-f-l form) row cols) (good-rows (or-f-r form) row cols))]
        [(not-f? form) (not (good-rows (not-f-e form) row cols)) ]
        [(eq-f? form) 
            (define idx (index-of (eq-f-name form) cols))
            (equal? (list-ref row idx) (eq-f-val form)) ] 
        [(eq2-f? form) 
            (define idx1 (index-of (eq2-f-name form) cols))
            (define idx2 (index-of (eq2-f-name2 form) cols))
            (equal? (list-ref row idx1) (list-ref row idx2))]
        [(lt-f? form)
            (define idx (index-of (lt-f-name form) cols))
            (< (list-ref row idx) (lt-f-val form))] ))

(define (table-select form tab) 
    (define rows (table-rows tab))
    (define colnames (get-colnames tab))
    (define (pom form row rows xs)
        (cond [(null? rows) 
                (if (good-rows form row colnames) (append xs (list row)) xs)]
            [(good-rows form row colnames)
                (pom form (car rows) (cdr rows) (append xs (list row)))]
            [else (pom form (car rows) (cdr rows) xs)]
        ))
    (pom form (car rows) (cdr rows) '()))

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
    (table tmp (table-rows tab)))

; Złączenie kartezjańskie

(define (make-connected-row row1 rows)
    (define (help row rows xs)
        (if (null? rows) 
            (append xs (list (append row1 row)))
            (help (car rows) (cdr rows) (append xs (list (append row1 row))))
        )
    )
    (help (car rows) (cdr rows) '()))

(define (table-cross-join tab1 tab2) 
    (define rows1 (table-rows tab1))
    (define rows2 (table-rows tab2))
    (define (pom row rows xs)
        (if (null? rows) 
            (append xs (make-connected-row row rows2))
            (pom (car rows) (cdr rows) (append xs (make-connected-row row rows2)))   
        ))
    (define new-rows (pom (car rows1) (cdr rows1) '()))
    (table 
        (append (table-schema tab1) (table-schema tab2)) 
        new-rows)
)

; Złączenie

(define (table-natural-join tab1 tab2) null)

; funkcje dodatkowe 

(define (get-colnames tab)
    (define x (table-schema tab))
    (define (pom xs x schema)
        (if (null? schema) 
            (append xs (list (column-info-name x)))
            (pom (append xs (list (column-info-name x))) (car schema) (cdr schema))))
    (pom '() (car x) (cdr x)))


(define (is-in? elem xs)
    (define (pom el x arr)
        (cond [(null? arr) (equal? el x)]
            [(equal? el x) #t]
            [(not (null? arr)) (pom el (car arr) (cdr arr))]
            [else #f]))
    (pom elem (car xs) (cdr xs))
)
(define (index-of x xs)
    (define (loop xs i)
        (cond [(null? xs) #f]
            [(equal? (car xs) x) i]
            [else (loop (cdr xs) (+ 1 i))]))
    (loop xs 0)
    )

(define (indexes cols tab)
    (define schema (get-colnames tab))
    (define (for sh xs idx)
        (if (null? xs) 
            (if (is-in? sh cols)
                (append idx (list 1))
                (append idx (list 0)))
            (if (is-in? sh cols)
                (for (car xs) (cdr xs) (append idx (list 1)))
                (for (car xs) (cdr xs) (append idx (list 0)))))
    )
    (for (car schema) (cdr schema) '()))
