(import (chicken process-context)
        (chicken string)
        (chicken load)
        (chicken file))

(define (path->command-name path)
    (substring path 0 (- (string-length path) 4)))

(define (list-commands)
    (map print
        (map path->command-name (directory "tools"))))

(let* ((args (command-line-arguments)))
    (if (< (length args) 1)
        (begin
            (print "Available commands:")
            (list-commands))
        (let* ((filename (car args))
               (remaining-args (cdr args)))
            (load (conc "tools/" filename ".scm"))
            (eval '(import tool))
            (if (procedure? (eval 'main))
                ((eval 'main) remaining-args)
                (begin
                    (print "Error: 'main' is not defined in the loaded file")
                    (exit 1)))))
)

