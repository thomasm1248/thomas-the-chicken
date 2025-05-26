(requires-extension srfi=13)

(module 4st
    (file->4st 4st->file)

    (import
        (chicken base)
		(chicken io)
        scheme)

; Utils

	(define (file->lines path)
		(let* ((port (open-input-file path))
			   (lines (read-lines port)))
			(close-input-port port)
			lines))

	(define (box value)
		(value))

	(define (box-get box)
		(car box))

	(define (box-set! box value)
		(set-car! box value))

	(define (stack list)
		(box list))

	(define (stack-peek stack)
		(car (box-get stack)))

	(define (stack-pop! stack)
		(let* ((list (box-get stack))
			   (top-item (car list)))
			(box-set! stack (cdr list))
			top-item))

	(define (stack-empty? stack)
		(= (stack-peek stack) '()))

; Parsing

	(define (parse-item lines level)
		(let ((line (remove-indentation level (stack-pop! lines)))
			  (location-of-space (string-contains line " "))
			  (name (if location-of-space
						(substring line 0 location-of-space)
						line))
			  (args (if location-of-space
						(substring line (+ location-of-space 1))
						"")))
			(name args
				(parse-items lines (+ level 1)))))
	
	(define (parse-items lines level)
		(if (stack-empty? lines)
			'()
			(cons
				(parse-item lines level)
				(parse-items lines level))))

; Exports

    (define (file->4st path)
		(parse-items 
			(stack (file->lines path))
			0))

    (define (4st->file tree path)
        (print "Not printing the following 4st to any file:")
        (print tree))
)
