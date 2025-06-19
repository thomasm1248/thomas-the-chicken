(module tool
    (main)
    (import
        scheme
        (chicken base)
        shell
        (chicken string)
        (chicken process-context))

    (define (main args)
        (if (= (length args) 0)
            (begin
                (print "Handles the t system.")
                (print "c -> recompile t.scm (only on Linux)"))
            (let ((command (car args)))
                (cond 
                    ((string=? command "c") (compile))
                    (else (print
                        (conc command " is not a recognized command.")))))))

    (define (compile)
        (print "Compiling t.scm -> bin/t")
        (let ((status (run* (csc -o bin/t core/t.scm >nul 2>&1))))
			(if (not (= status 0))
				(print "Compile failed")
				(print "Done"))))

)
