(import
    (prefix sdl2 "sdl2:")
    (rename (chicken random)
        (pseudo-random-integer rand))
    scheme
    (chicken base))

(sdl2:set-main-ready!)
(sdl2:init! '(video))
(on-exit sdl2:quit!)

(define window (sdl2:create-window! "Hello, World!" 0 0 600 400))
(define win-surf (sdl2:window-surface window))

(define (make-rand-color)
    (sdl2:make-color
        (rand 256)
        (rand 256)
        (rand 256)))

(do ((i 0 (+ i 1)))
        ((= i 100))
    (let ((col-arbitrary (make-rand-color)))
        (sdl2:fill-rect! win-surf #f col-arbitrary)
        (sdl2:free-color! col-arbitrary)
        (sdl2:update-window-surface! window)
        (sdl2:delay! 500)))
    
