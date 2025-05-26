(load "utils/4st.scm")

(module tool
	(main)

	(import
		scheme
		(chicken string)
		(chicken base)
		4st)

	(define (main args)
		(print (conc
			"Returned: "
			(file->4st "config.4st"))))
)
