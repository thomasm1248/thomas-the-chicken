(module tool
    (main)
    (import
		scheme
		(chicken base)
		shell
		(chicken string)
		(chicken process-context))

    (define (compile)
        (print "Compiling t.scm -> bin/t")
        (let ((status (run* (csc -o bin/t core/t.scm >nul 2>&1))))
			(if (not (= status 0))
				(print "Compile failed")
				(print "Done"))))

    (define (main args)
        (if (= (length args) 0)
            (print "No arguments were supplied to sys")
            (let ((command (car args)))
                (cond 
                    ((string=? command "c") (compile))
                    (else (print
                        (conc command " is not a recognized command.")))))))
)
