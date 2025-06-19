(load "streams.scm")
(import
    (prefix streams "s-")
    scheme
    (chicken base))

(define a (s-make))
(define b (s-make))

; union

; where

; select

; combine

(define comb (s-combine a b))
(s-sub! comb (lambda (val)
    (print val)))

; change




; send values through the streams

(s-send! a 1)
(s-send! b 2)
(s-send! a 3)
(s-send! b 4)
(s-send! a 5)
(s-send! b 6)
(s-send! a 7)
