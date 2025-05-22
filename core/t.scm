(import (chicken process-context)
        (chicken string)
        (chicken load))

(let* ((args (command-line-arguments)))
    (if (< (length args) 1)
        (begin
            (print "Usage: t <tool> [args...]")
            (exit 1))
        (let* ((filename (car args))
               (remaining-args (cdr args)))
            (load (conc "tools/" filename ".scm"))
            (eval '(import tool))
            (if (procedure? (eval 'main))
                ((eval 'main) remaining-args)
                (begin
                    (print "Error: 'main' is not defined in the loaded file")
                    (exit 1))))))

