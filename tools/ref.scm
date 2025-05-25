(module tool
    (main)

    (import
        (chicken string)
        scheme
        (chicken base))

    (define (add-ref tag resource)
        (print (conc
            "Adding ref for '" tag "' "
            "that points to " resource))
        (print "jk, that functionality hasn't been added yet. sorry."))

    (define (main args)
        (if (= (length args) 0)
            (begin
                (print "Manages references that my books make to digital content.")
                (print "add [tag] [path|url] -> add a new reference to the repository"))
            (let ((command (car args)))
                (cond
                    ((string=? command "add")
                        (let ((remaining (cdr args)))
                            (if (= (length remaining) 2)
                                (add-ref
                                    (car remaining)
                                    (car (cdr remaining)))
                                (print "'add' requires two arguments"))))
                    (else
                        (print (conc "'" command "' is not recognized")))))))
)
