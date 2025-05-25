(module 4st
    (file->4st 4st->file)

    (import
        (chicken base)
        scheme)

    (define (file->4st path)
        (print "Not reading 4st from file:")
        (print path)
        (print "Returning () instead")
        '())

    (define (4st->file tree path)
        (print "Not printing the following 4st to any file:")
        (print tree))
)
