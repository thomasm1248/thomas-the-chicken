(module tool
    (main)
    (import scheme (chicken base))

    (define (main args)
        (if (= (length args) 0)
            (print "No arguments were supplied to sys")
            (case (car args)
                (else (print "sys has no functionality yet.")))))
)
