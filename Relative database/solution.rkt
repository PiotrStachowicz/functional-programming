#lang racket
; Piotr Stachowicz 337942
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

; Funkcja potrzebna do map
(define (append-lists xs)
  (if (empty? xs) empty
      (append (car xs) (append-lists (cdr xs)))))

; Konkatenacja dla symboli, można dzięki niej także tworzyć predykaty
(define (append-letter x sym)
  (cond [(equal? x "?")  (eval (string->symbol (string-append (symbol->string sym) x)))]
        [else (string->symbol (string-append (symbol->string sym) x))]))

; Przykładowe tabele
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

; check-row sprawdza czy zgadza się ilość komórek oraz ich typ w wierszu
(define (check-row row sch)
  (cond [(and (null? row) (null? sch)) #t]
        [(or (null? row) (null? sch)) (error "Zła ilość kolumn!")]
        [((append-letter "?" (column-info-type (car sch))) (car row)) (check-row (cdr row) (cdr sch))]
        [else (error "Zły typ danych!")]))

(define (table-insert row tab)
  (if (check-row row (table-schema tab))
      (table (table-schema tab) (cons row (table-rows tab))) (error "Błąd podczas wstawiania!")))

; Projekcja

; index-chooser tworzy listę indeksów podanych kolumn (w podanej przez użytkownika kolejności)
(define (index-chooser sch names)
  (define (helper sch-copy names idx)
    (cond [(null? names) '()]
          [(null? sch-copy) (error "Nie istnieje taka kolumna!")]
          [(equal? (column-info-name (car sch-copy)) (car names)) (cons idx (helper sch (cdr names) 0))]
          [else (helper (cdr sch-copy) names (+ 1 idx))]))
  (helper sch names 0))

; column-row-chooser tworzy listę kolumn/wierszy zadaną przez indeksy kolumn
(define (column-row-chooser sch idxs)
  (define (helper sch-copy idx)
    (cond [(null? idxs) '()]
          [(null? sch-copy) (error "Nie istnieje taka kolumna!")]
          [(equal? idx (car idxs)) (cons (car sch-copy) (column-row-chooser sch (cdr idxs)))]
          [else (helper (cdr sch-copy) (+ 1 idx))]))
  (helper sch 0))

(define (table-project cols tab)
  (define idxs (index-chooser (table-schema tab) cols))
  (table (column-row-chooser (table-schema tab) idxs)
         (map (lambda (x) (column-row-chooser x idxs)) (table-rows tab))))

; Sortowanie

; compare-to sprawdza która komórka jest większa
(define (compare-to kom1 kom2)
  (cond [(and (string? kom1) (string? kom2)) (string<? kom1 kom2)]
        [(and (boolean? kom1) (boolean? kom2)) (and (not kom1) kom2)]
        [(and (symbol? kom1) (symbol? kom2)) (string<? (symbol->string kom1) (symbol->string kom2))]
        [(and (number? kom1) (number? kom2)) (< kom1 kom2)]
        [else (error "Zły typ danych")]))

; compare-row sprawdza który wiersz jest większy
(define (compare-row row1 row2 idxs)
  (define (helper row1-copy row2-copy idxs idx)
    (cond [(null? idxs) #f]
          [(or (null? row1-copy) (null? row2-copy)) (helper row1 row2 (cdr idxs) 0)]
          [(equal? idx (car idxs))
           (if (equal? (car row1-copy) (car row2-copy))
               (helper row1 row2 (cdr idxs) 0)
               (compare-to (car row1-copy) (car row2-copy)))]
          [else (helper (cdr row1-copy) (cdr row2-copy) idxs (+ idx 1))]))
  (helper row1 row2 idxs 0))

; curry przygotowywuje compare-row do użycia w sort'cie
(define (compare-rows-curry idxs)
    (lambda (x y) (compare-row x y idxs)))

(define (table-sort cols tab)
  (table (table-schema tab) (sort (table-rows tab) (compare-rows-curry (index-chooser (table-schema tab) cols)))))

; Selekcja

(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))

; value-name (zwraca wartość w wierszu pod nazwą name)
(define (value-name name row sch)
  (cond [(or (null? sch) (null? row)) (error "Bledne dane!")]
        [(equal? name (column-info-name (car sch))) (car row)]
        [else (value-name name (cdr row) (cdr sch))]))

; checks-out? sprawdza czy wiersz spełnia formułę form
(define (checks-out? form row tab)
  (cond [(and-f? form) (and (checks-out? (and-f-l form) row tab) (checks-out? (and-f-r form) row tab))]
        [(or-f? form) (or (checks-out? (or-f-l form) row tab) (checks-out? (or-f-r form) row tab))]
        [(not-f? form) (not (checks-out? (not-f-e form) row tab))]
        [(eq-f? form) (equal? (value-name (eq-f-name form) row (table-schema tab)) (eq-f-val form))]
        [(eq2-f? form) (equal? (value-name (eq2-f-name form) row (table-schema tab))
                               (value-name (eq2-f-name2 form) row (table-schema tab)))]
        [(lt-f? form) (compare-to (value-name (lt-f-name form) row (table-schema tab)) (lt-f-val form))]
        [else (error "Nieobsługiwany predykat!")]))

(define (table-select form tab)
  (table (table-schema tab) (filter (lambda (x) (checks-out? form x tab)) (table-rows tab))))

; Zmiana nazwy

(define (table-rename col ncol tab)
  (define (table-rename-helper column new-column sch)
    (cond [(null? sch) (error "Nie istnieje taka kolumna!")]
          [(equal? column (column-info-name (car sch))) (cons (column-info new-column (column-info-type (car sch))) (cdr sch))]
          [else (cons (car sch) (table-rename-helper column new-column (cdr sch)))]))
  (if (or (not (symbol? col)) (not (symbol? ncol))) (error "Nazwa musi być symbolem!")
      (table (table-rename-helper col ncol (table-schema tab)) (table-rows tab))))

; Złączenie kartezjańskie

(define (table-cross-join tab1 tab2)
  (table (append (table-schema tab1) (table-schema tab2))
         (append-lists (map (lambda (x) (map (lambda (y) (append x y)) (table-rows tab2))) (table-rows tab1)))))

; Złączenie naturalne

; column-names-list tworzy listę zawierającą nazwy poszczególnych kolumn
(define (column-names-list sch)
  (foldr (lambda (x y) (cons (column-info-name x) y)) '() sch))

; duplicates-names tworzy listę duplikatów nazw poszczególnych kolumn obu tablic
(define (duplicates-names tab1 tab2)
  (filter (lambda (x) (member x  (column-names-list (table-schema tab2)))) (column-names-list (table-schema tab1))))

; change-names zmienia nazwy podanych w liście kolumn
(define (change-names tab2 cols ncols)
  (cond [(or (null? cols) (null? ncols)) tab2]
        [else (change-names (table-rename (car cols) (car ncols) tab2) (cdr cols) (cdr ncols))]))

; form-creator tworzy formułe potrzebną do wybrania odpowiednich wierszy
(define (form-creator duplicates ncols)
  (cond [(null? (cdr duplicates)) (eq2-f (car duplicates) (car ncols))]
        [else (and-f (eq2-f (car duplicates) (car ncols)) (form-creator (cdr duplicates)))]))

; Używam funkcji wbudowanej gensym aby stworzyć unikatową nazwę dla kolumn (była pokazana jako ciekawostka na wykładzie 12.04)
; table-natural-join złącza listy według schematu z polecenia
(define (table-natural-join tab1 tab2)
  (let* ([duplicates (duplicates-names tab1 tab2)]
         [new-names (map (lambda (x) (gensym)) duplicates)]
         [columns-for-project (remove-duplicates (column-names-list (append (table-schema tab1) (table-schema tab2))))]
         [tab2 (change-names tab2 duplicates new-names)]
         [tab2 (table-cross-join tab1 tab2)]
         [rows (table-rows (table-select (form-creator duplicates new-names) tab2))]
         [tab2 (table (table-schema tab2) rows)])
    (table-project columns-for-project tab2)))