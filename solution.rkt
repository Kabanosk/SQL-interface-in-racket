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

(define (empty-table columns) (table columns '()))

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
    (cond [(null? row) #f]
        [(null? tab) #f]
        [(not (equal? (length row) (length types))) #f]
        [else
        (define (check-types x row type types)
            (define f (type-question type))
            (if (null? types) 
                (f x)
                (if (not (f x)) 
                    #f
                    (check-types (car row) (cdr row) (car types) (cdr types)))  
    ))
    (check-types (car row) (cdr row) (car types) (cdr types))])
)
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

(define (grater-function elem)
    (cond [(eq? elem 'string) string<? ]
         [(eq? elem 'number) <]
         [(eq? elem 'boolean) (lambda (x y) (implies x y))]  
         [(eq? elem 'symbol) (lambda (x y) (string<? (symbol->string x) (symbol->string y)))]
  )
)

(define (find-grater-function elem cols)
    (cond [(null? cols) (error "Not found")]
        [(eq? elem (column-info-name (car cols)))
            (grater-function (column-info-type (car cols)))]
        [else (find-grater-function elem (cdr cols))])
)

;;getting element from column of name col
(define (get-elem elem col cols) 
  (if (eq? col (column-info-name (car cols)))
      (car elem)
      (get-elem (cdr elem) col (cdr cols))))

;;compares two rows to find the smaller one
(define (compare cols new old schema)
  (cond [(null? cols) #f]
        [(eq? (get-elem new (car cols) schema) (get-elem old (car cols) schema)) 
            (compare (cdr cols) new old schema)]
        [else 
            (define f (find-grater-function (car cols) schema))
            (f (get-elem new (car cols) schema) (get-elem old (car cols) schema))]))

(define (add cols new rows schema)
  (if (null? rows)
      (list new)
      (if (compare cols new (car rows) schema)
          (cons new rows)
          (cons (car rows) (add cols new (cdr rows) schema)))))

(define (table-sort cols tab) 
    (define (sorted-tab new-tab schema cols rows)
        (if (null? rows)
            new-tab
            (sorted-tab 
                (table (table-schema new-tab) 
                        (add cols (car rows) (table-rows tab) (table-schema tab) )) 
                schema cols (cdr rows))))
    (sorted-tab 
        (empty-table (table-schema tab))
        (table-schema tab) 
        cols 
        (table-rows tab)))

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
    (define new-rows (pom form (car rows) (cdr rows) '()))
    (table (table-schema tab) new-rows)
    )

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
        new-rows))

; Złączenie

(define (table-natural-join tab1 tab2) 
    (define sch1 (get-colnames tab1))
    (define sch2 (get-colnames tab2))
    (define (pom schema acc)
        (if (null? schema) 
            acc
            (pom 
                (cdr schema) 
                (if (is-in? (car schema) sch1) (append acc (list (car schema))) acc))))
    (define rep (pom sch2 '()))
    (define (pom1 org-cols cols-to-rename tab)
        (if (null? cols-to-rename)
            tab
            (pom1 (cdr org-cols) (cdr cols-to-rename) 
                (table-rename (car org-cols) (string->symbol (string-append (symbol->string (car cols-to-rename)) "1")) tab))
        )
    )
    (define (change-rep xs acc)
        (if (null? xs) acc
        (change-rep (cdr xs) (append acc (list (string->symbol (string-append (symbol->string (car xs)) "1"))) )
    )))
    (define new-tab-cart (table-cross-join tab1 (pom1 sch2 rep tab2)))
    (define i0 (indexes rep new-tab-cart))
    (define i1 (indexes (change-rep rep '()) new-tab-cart))
    (define (indexes-natural xs i acc)
        (cond [(null? xs) acc] 
            [(equal? (car xs) 1) (indexes-natural (cdr xs) (add1 i) (append acc (list i)))]
            [else (indexes-natural (cdr xs) (add1 i) acc)])
    )
    (define (is-good-row? row xs ys)
        (cond [(null? xs) #t]
            [(equal? (list-ref row (car xs)) (list-ref row (car ys))) (is-good-row? row (cdr xs) (cdr ys))]
            [else #f]
        )
    )
    (define idxs0 (indexes-natural i0 0 '()))
    (define idxs1 (indexes-natural i1 0 '()))
    (define (good-rows-natural rows acc)
        (cond [(null? rows) acc]
            [(is-good-row? (car rows) idxs0 idxs1) (good-rows-natural (cdr rows) (append acc (list (car rows))))]
            [else (good-rows-natural (cdr rows) acc)]
        )
    )
    (define new-schema (table-schema new-tab-cart))
    (define new-tab (table new-schema (good-rows-natural (table-rows new-tab-cart) '())))
    (define (all-except i idxs acc) 
        (cond [(null? idxs) acc]
            [(equal? (car idxs) 0) (all-except (add1 i) (cdr idxs) (append acc (list (column-info-name (list-ref new-schema i)))))]
            [else (all-except (add1 i) (cdr idxs) acc)]
        )
        )
    (table-project (all-except 0 i1 '()) new-tab)
)

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
    (pom elem (car xs) (cdr xs)))

(define (index-of x xs)
    (define (loop xs i)
        (cond [(null? xs) #f]
            [(equal? (car xs) x) i]
            [else (loop (cdr xs) (+ 1 i))]))
    (loop xs 0))

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
