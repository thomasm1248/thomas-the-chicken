(load "utils/csv.scm")
(load "utils/config.scm")

(module tool
    (main)

    (import

        ; my libraries
        (prefix csv csv:)
        (prefix config config:)

        (prefix (chicken file) file:)
        (chicken pathname)
        (srfi 113) ; sets
        (srfi 128) ; comparators (for sets)
        (chicken string)
        shell
        scheme
        (chicken base))

    (define finance-path (config:get "Finances" "DataDirectory"))

    (define (main args)
        (if (= (length args) 0)
            (begin
                (print "Manages finances"))
            (let ((command (car args)))
                (cond
                    ((string=? command "import")
                        (if (= (length args) 2)
                            (import-data (cadr args))
                            (print "1 argument: path to exported csv")))
                    ((string=? command "data")
                        (print-data (cdr args)))
                    (else (print
                        (conc command " is not a recognized command.")))))))

    (define (print-data args)
        (if (= (length args) 0)
            (begin
                (print "String together any of the following commands:")
                (print "  show - print the data in its current form"))
            (let ((data (get-all-data)))
                (for-each process-transaction! data)
                (execute-data-commands data args))))

    (define (import-data filepath)
        (if (not finance-path)
            (print "Required config: Finances -> DataDirectory")
            (run (mv ,filepath ,(conc finance-path "/raw")))))
    
    ;;;;;;;;;;;;;;; Core Functions

    ;;; DATA commands

    (define (execute-data-commands data commands)
        (cond
            ((= (length commands) 0)
                data)
            ((string=? (car commands) "show")
                (print-transactions data)
                (execute-data-commands data (cdr commands)))
            (else
                (display "Unrecognized command: ")
                (print (car commands)))))

    ;;; top-level: get all the data from all files

    (define (get-all-data)
        (let ((autumn-data (get-data "autumn"))
              (thomas-data (get-data "thomas")))
            (let ((autumn-transactions
                    (map translate-autumn-data autumn-data))
                  (thomas-transactions
                    (map translate-thomas-data thomas-data)))
                (append autumn-transactions thomas-transactions))))

    ;;; Process data

    (define (process-transaction! transaction)
        (if (not (transaction-category transaction))
            (set-transaction-category! transaction "Uncategorized"))
    )

    ;;; Translate data

    ; end goal: transaction
    (define-record-type transaction
        (make-transaction date account category amount description)
        transaction?
        (date transaction-date set-transaction-date!); M/D/YYYY
        (account transaction-account set-transaction-account!); string
        (category transaction-category set-transaction-category!); string | #f
        (amount transaction-amount set-transaction-amount!); number (+add -remove)
        (description transaction-description set-transaction-description!)); string

    (define (translate-thomas-data row)
        (make-transaction
            ; date
            (string->date (list-ref row 0))
            ; account
            (list-ref row 1)
            ; category
            (list-ref row 4)
            ; amount
            (string->number (list-ref row 7))
            ; description
            (list-ref row 2)))

    (define (translate-autumn-data row)
        (make-transaction
            ; date
            (string->date (list-ref row 0))
            ; account
            "Autumn's Checking"
            ; category
            #f
            ; amount
            (+ (or (string->number (list-ref row 3)) 0)
               (or (string->number (list-ref row 4)) 0))
            ; description
            (list-ref row 2)))

    ;;; Parse CSV from files

    (define (get-data prefix)
        (let ((unique-lines (set string-comparator))
              (files (get-files-as-lines prefix)))
            (for-each (lambda (file-lines)
                    (list->set! unique-lines file-lines))
                files)
                ; TODO Growth Story by Kupla
            (csv:lines->data (set->list unique-lines))))

    (define (get-files-as-lines prefix)
        (file:find-files
            (conc finance-path "/raw/")
            #:test
                (lambda (path)
                    (starts-with (pathname-strip-directory path) prefix))
            #:action
                (lambda (curr all)
                    ; TODO Nervous Ticks by Maribou State
                    (cons
                        (cdr (csv:file->lines curr)) ; all but first line
                        all))
            #:limit
                (constantly #f)))

    ;;;;;;;;;;;;;;; Util functions

    ; used for making string sets
    (define string-comparator (make-comparator string? string=? string<? string-hash))

    (define (starts-with main-string prefix)
        (substring=?
            main-string
            prefix))
            
    (define (print-table table)
        (for-each (lambda (row)
            (for-each (lambda (cell)
                (display cell)
                (display "     "))
                row)
            (print ""))
            table))

    (define (print-transactions transactions)
        (for-each (lambda (transaction)
            (display (date-year (transaction-date transaction)))
            (display "/")
            (display (date-month (transaction-date transaction)))
            (display "/")
            (display (date-day (transaction-date transaction)))
            (display " - ")
            (display (pad-right (transaction-account transaction) 20))
            (display " ")
            (display (transaction-amount transaction))
            (display "     ")
            (let ((cat (transaction-category transaction)))
                (if cat
                    (display cat)))
            (display "     ")
            (print (transaction-description transaction)))
            transactions))

    (define (pad-right str target-length)
        (let* ((current-length (string-length str))
               (pad-length (max 0 (- target-length current-length)))
               (spaces (make-string pad-length #\space)))
        (string-append str spaces)))

    (define-record-type date
        (make-date year month day)
        date?
        (year date-year)
        (month date-month)
        (day date-day))
    (define (string->date text)
        (let ((parts (string-split text "/")))
            (make-date
                (string->number (list-ref parts 2))
                (string->number (list-ref parts 0))
                (string->number (list-ref parts 1)))))

)
