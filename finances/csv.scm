(module csv
    (read-file)

    ;; Requires srfi-13 for string splitting
    (import
        scheme
        ;(srfi 13) TODO check if this actually does anything
        (chicken io))

    ;; Read CSV file into list of rows
    (define (read-file filename)
        (let ((lines (file->lines filename)))
            (map (lambda (line)
                    (parse-csv-line line))
                lines)))

    ;;;;;;;;; Helpers

    ;; Parse a single CSV line into a list of fields
    (define (parse-csv-line line)
      (let loop ((chars (string->list line))
                 (field '())
                 (fields '())
                 (in-quotes? #f))
        (cond
         ((null? chars)
          (reverse (cons (list->string (reverse field)) fields)))
         ((and (char=? (car chars) #\,) (not in-quotes?))
          (loop (cdr chars) '() (cons (list->string (reverse field)) fields) in-quotes?))
         ((char=? (car chars) #\")
          (loop (cdr chars) field fields (not in-quotes?)))
         (else
          (loop (cdr chars) (cons (car chars) field) fields in-quotes?)))))

    ;; Read a list of lines from a file
    (define (file->lines path)
        (let* ((port (open-input-file path))
               (lines (read-lines port)))
            (close-input-port port)
            lines))
)
