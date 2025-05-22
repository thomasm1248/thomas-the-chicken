(module tool
    (main)
    (import scheme (chicken base) shell (chicken string))

    (define (compile)
        (print "Compiling t.scm -> bin/t")
        (run (csc -o bin/t core/t.scm))
        (print "Done"))

    (define (main args)
        (if (= (length args) 0)
            (print "No arguments were supplied to sys")
            (let ((command (car args)))
                (cond 
                    ((string=? command "c") (compile))
                    (else (print
                        (conc command " is not a recognized command.")))))))
)
