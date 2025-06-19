(module 4st
    (file->4st 4st->file)

    (import
        (chicken base)
        (chicken io)
        (srfi 13) ; strings TODO try (chicken string)
        scheme)

    (define (file->4st path)
        (car
            (parse-items 0
                (file->lines path))))

    (define (4st->file tree path)
        (print "Not printing the following 4st to any file:")
        (print tree))

    ;;;;;;;;;;;;;;;; Utils

    (define (file->lines path)
        (let* ((port (open-input-file path))
               (lines (read-lines port)))
            (close-input-port port)
            lines))

    (define (first-non-space-index str)
        (let loop ((i 0))
            (cond
                ((>= i (string-length str)) #f)
                ((char-whitespace? (string-ref str i)) (loop (+ i 1)))
                (else i))))

    (define (remove-indentation level line)
        (let ((start-index (first-non-space-index line)))
            (assert (= (* level 4) start-index))
            (substring line start-index)))

    ;;;;;;;;;;;;;;;;; Parsing

    (define (parse-item level lines); -> (list item remaining-lines)
        (let* ((line (remove-indentation level (car lines)))
               (location-of-space (string-contains line " "))
               (name (if location-of-space
                        (substring line 0 location-of-space)
                        line))
               (args (if location-of-space
                        (substring line (+ location-of-space 1))
                        "")))
            (let* ((results (parse-items (+ level 1) (cdr lines)))
                   (items (car results))
                   (remaining-lines (cadr results)))
                (list
                    (append (list name args '()) items)
                    remaining-lines))))
    
    (define (parse-items level lines); -> (list items remaining-lines)
        (if (null? lines)
            (list '() '())
            (let* ((results (parse-item level lines))
                   (item (car results))
                   (remaining-lines (cadr results)))
                (let* ((results (parse-items level remaining-lines))
                       (items (car results))
                       (remaining-lines (cadr results)))
                    (list
                        (cons item items)
                        remaining-lines)))))
)
