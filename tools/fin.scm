(load "utils/csv.scm")
(load "utils/config.scm")

(module tool
    (main)

    (import
        (prefix csv "csv:") ; my library
        (prefix config "config:")
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
                    (else (print
                        (conc command " is not a recognized command.")))))))

    (define (import-data filepath)
        (if (not finance-path)
            (print "Required config: Finances -> DataDirectory")
            (run (mv ,filepath ,(conc finance-path "/raw")))))
    
    ;;;;;;;;;;;;;;; Util functions

    ; used for making string sets
    (define string-comparator (make-comparator string? string=? string<? string-hash))
            
)
