(load "mosaic.scm")
(import
    (prefix mosaic "mos-")
    (chicken base)
    scheme)

(mos-init! "Test")

(define panel (mos-panel! "Test"
    (mos-buttons
        (lambda (func) (func))
        (list "Test" (lambda () (print "Test")))
        (list "Foo"  (lambda () (print "Foo")))
        (list "Bar"  (lambda () (print "Bar"))))))

(define panel (mos-panel! "Test2"
    (mos-buttons
        (lambda (string) (print string))
        (list "Test" "Test")
        (list "Foo" "Foo")
        (list "Bar" "Bar"))))

(do () (#f)
    (mos-draw-loop! 30))
