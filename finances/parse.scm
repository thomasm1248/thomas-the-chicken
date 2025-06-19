(load "csv.scm")
(import
    (prefix csv "csv.")
    scheme
    (srfi 113) ; sets
    (srfi 128) ; comparators (for sets)
    (chicken base))

(define data (csv.read-file "transactions-2025-06-16.csv"))

(define string-comparator (make-comparator string? string=? string<? string-hash))

(define cats (list->set string-comparator
    (map (lambda (row)
            (list-ref row 4))
        (cdr data))))

(set-for-each (lambda (category)
    (print category))
    cats)
