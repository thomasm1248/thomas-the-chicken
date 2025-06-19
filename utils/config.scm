(load "utils/4st.scm")

(module config
    (get)

    (import
        4st ; my library
        shell
        scheme
        (chicken base))

    (define (get . path)
        (let ((data (file->4st "config.4st")))
            (apply get-from-items data path)))

    (define (get-from-items items . path)
        (if (null? items)
            #f
            (let ((item (car items)))
                (if (string=? (car item) (car path))
                    (apply get-from-item item (cdr path))
                    (apply get-from-items (cdr items) path)))))

    (define (get-from-item item . path)
        (if (null? path)
            (cadr item)
            (apply get-from-items (cdddr item) path)))
)
