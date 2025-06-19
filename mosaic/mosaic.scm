(load "streams.scm")

(module mosaic
    (init! draw-loop!
     panel!
     buttons)

    (import
        (prefix streams stream:)
        (prefix sdl2 sdl2:)
        (prefix sdl2-ttf ttf:)
        scheme
        miscmacros
        (chicken base))

    ;;;;;;;;;;; Types

    (define-record-type :panel
        (make-panel name type children x y w h prune? dirty?)
        panel?
        (name panel-name) ; string
        (type panel-type) ; 'regular 'horizontal
        (children panel-children panel-children-set!)
        (x panel-x panel-x-set!)
        (y panel-y panel-y-set!)
        (w panel-w panel-w-set!)
        (h panel-h panel-h-set!)
        (prune? panel-prune? panel-prune?-set!)
        (dirty? panel-dirty? panel-dirty?-set!))

    (define-record-type :button
        (make-button label value x y w h)
        button?
        (label button-label set-button-label!)
        (value button-value)
        (x button-x button-x-set!)
        (y button-y button-y-set!)
        (w button-w button-w-set!)
        (h button-h button-h-set!))

    (define-record-type :buttons
        (make-buttons handler buttons x y w h stretch? dirty?)
        buttons?
        (buttons buttons-buttons buttons-buttons-set!)
        (handler buttons-handler)
        (x buttons-x buttons-x-set!)
        (y buttons-y buttons-y-set!)
        (w buttons-w buttons-w-set!)
        (h buttons-h buttons-h-set!)
        (stretch? buttons-stretch? buttons-stretch?-set!)
        (dirty? buttons-dirty? buttons-dirty?-set!))

    (define (component? object)
        (or
            (buttons? object)))

    ;;;;;;;;;;; Globals

    ; drawing management
    (define window #f)
    (define root #f)
    (define rescan-needed? #t)
    (define redraw-needed? #t)

    ; spacing
    (define panel-margin 8)
    (define button-padding 3)

    ; font
    (define font #f)
    (define font-size 15)

    ; colors
    (define background-color (sdl2:make-color 100 100 100))
    (define panel-color (sdl2:make-color 150 150 150))
    (define button-color (sdl2:make-color 200 200 200))
    (define text-color (sdl2:make-color 0 0 0))

    ;;;;;;;;;;; Utils

    (define (partition size partitions)
        (if (= partitions 1)
            (list size)
            (let ((this-partition (quotient size partitions)))
                (cons this-partition
                    (partition (- size this-partition) (- partitions 1))))))

    (define (filter pred lst)
        (cond
            ((null? lst) '())
            ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
            (else (filter pred (cdr lst)))))

    ;;;;;;;;;;; System Utils
    
    (define (request-rescan)
        (set! rescan-needed? #t))

    (define (request-redraw)
        (set! redraw-needed? #t))

    ;;;;;;;;;;; Functions

    ; Buttons

    (define (rescan-buttons! buttons)
        (buttons-dirty? buttons))

    (define (render-buttons-text button-list max-w)
        (map (lambda (button)
                (ttf:render-utf8-shaded font (button-label button)
                    text-color button-color))
            button-list))

    (define (soft-render-buttons! buttons surface)
        (if (buttons-dirty? buttons)
            (render-buttons! buttons surface #f #f #f #f #f)))

    (define (render-buttons! buttons surface x y w h stretch?)
        (buttons-dirty?-set! buttons #f) ; clear flag
        (if x
            ; update dimensions of panel
            (begin
                (buttons-stretch?-set! buttons stretch?)
                (buttons-x-set! buttons x)
                (buttons-y-set! buttons y)
                (buttons-w-set! buttons w))
                ; except for height. we won't know that 'till we're done
            ; use previous settings
            (begin
                (set! stretch? (buttons-stretch? buttons))
                (set! x (buttons-x buttons))
                (set! y (buttons-y buttons))
                (set! w (buttons-w buttons))
                (set! h (buttons-h buttons))))
        ; render the buttons
                ; render each button's text
        (let* ((text-surfs (render-buttons-text (buttons-buttons buttons) w))
                ; used for placing each button
               (dest (sdl2:make-rect 0 0 0 0))
                ; calculate height of each row
               (row-height (+ (* button-padding 2) (sdl2:surface-h (car text-surfs)))))
            (let ((bx x) (by y)) ; for positioning the buttons
                ; Draw each button
                (for-each (lambda (surf)
                        ; Move to next line if there's too little space
                        (let ((remaining-w (- w (- bx x)))
                              (button-width (+ (sdl2:surface-w surf)
                                               (* 2 button-padding))))
                            (if (> button-width remaining-w)
                                (begin
                                    (set! bx x)
                                    (set! by (+ by row-height)))))
                        (let ((bw (+ (sdl2:surface-w surf) (* button-padding 2)))
                              (bh row-height))
                            ; Draw button background
                            (sdl2:rect-x-set! dest bx)
                            (sdl2:rect-y-set! dest by)
                            (sdl2:rect-w-set! dest bw)
                            (sdl2:rect-h-set! dest bh)
                            (sdl2:fill-rect! surface dest button-color)
                            ; Draw the button
                            (sdl2:rect-x-set! dest (+ bx button-padding))
                            (sdl2:rect-y-set! dest (+ by button-padding))
                            (sdl2:blit-surface! surf #f surface dest)
                            ; Move to end of button
                            (set! bx (+ bx bw))))
                    text-surfs)
                ; return total height
                (let ((total-height (+ by row-height)))
                    ; free the allocated rect
                    (sdl2:free-rect! dest)
                    ; record the height of this component
                    (if stretch?
                        (buttons-h-set! buttons h)
                        (buttons-h-set! buttons total-height))
                    ; return
                    total-height))))

    (define (button-definitions->button-list defs)
        (map (lambda (definition)
            (let ((label (car definition))
                  (value (cadr definition)))
                (make-button label value 0 0 0 0)))
            defs))

    ; General Component functions
    
    (define (rescan-component! component)
        (cond
            ((buttons? component)
                (rescan-buttons! component))))

    (define (soft-render-component! component surface)
        (cond
            ((buttons? component)
                (soft-render-buttons! component surface))
            (else
                (print "unrecognized component"))))

    (define (render-component! component surface x y w h stretch?)
        (cond
            ((buttons? component)
                (render-buttons! component surface x y w h stretch?))
            (else
                (print "unrecognized component"))))

    ; Panels

    (define (prune-panel! panel)
        ; What kind of panel?
        (let ((type (panel-type panel)))
            ; Is it a regular panel?
            (if (eq? 'regular (panel-type panel))
                ; Yes. Does it need to be pruned?
                (if (panel-prune? panel)
                    #f ; prune it
                    panel) ; leave it be
                ; No. Check how many children it has
                (let ((original-num-childs (length (panel-children panel)))
                      (remaining-children
                            (filter prune-panel! (panel-children panel))))
                    ; If the number of children has changed, redraw
                    (if (not (= original-num-childs (length remaining-children)))
                        (panel-dirty?-set! panel #t))
                    ; What to do with this panel?
                    (cond
                        ; Have all children been pruned? prune this panel too
                        ((= (length remaining-children) 0) #f)
                        ; Is there only one child left? promote it and prune this one
                        ((= (length remaining-children) 1) (car remaining-children))
                        ; Otherwise, update panel and leave it be
                        (else
                            (panel-children-set! panel remaining-children)
                            panel))))))

    (define (rescan-panel! panel)
        ; What kind of panel?
        (let ((type (panel-type panel)))
            (cond
                ; Contains a component?
                ((eq? type 'regular)
                    ; Rescan the component
                    (rescan-component!
                        (car (panel-children panel)))
                    #f)
                ; Contains other panels?
                ((eq? type 'horizontal)
                    ; Rescan each panel
                    (for-each (lambda (child-panel)
                            (rescan-panel! child-panel))
                        (panel-children panel))
                    #f))))

    (define (soft-render-regular-panel! panel surface)
        (if (panel-dirty? panel)
            (render-regular-panel! panel surface #f #f #f #f)
            (soft-render-component! (car (panel-children panel)) surface)))

    (define (render-regular-panel! panel surface x y w h)
        (panel-dirty?-set! panel #f) ; clear flag
        (if x
            ; update dimensions of panel
            (begin
                (panel-x-set! panel x)
                (panel-y-set! panel y)
                (panel-w-set! panel w)
                (panel-h-set! panel h))
            ; use previous dimensions
            (begin
                (set! x (panel-x panel))
                (set! y (panel-y panel))
                (set! w (panel-w panel))
                (set! h (panel-h panel))))
        ; bother rendering?
        (cond
            ((< w 20) #f) ; not wide enough
            ((< h 20) #f) ; not tall enough
            (else
                ; yes. render
                (let ((rect (sdl2:make-rect x y w h)))
                    ; Render a background in case some space isn't used
                    (sdl2:fill-rect! surface rect panel-color)
                    ; Render the inner component in stretch mode
                    (render-component! (car (panel-children panel))
                        surface x y w h #t)
                    ; Free the rect
                    (sdl2:free-rect! rect)))))

    (define (render-panels-horizontally! panels partitions surface x y h)
        ; recursively render panels
        (if (> (length panels) 0)
            (begin
                (render-panel!
                    (car panels)
                    surface
                    x y (car partitions) h)
                (render-panels-horizontally! (cdr panels) (cdr partitions)
                    surface (+ x (car partitions)) y h))))

    (define (soft-render-horizontal-panel! panel surface)
        (if (panel-dirty? panel)
            (render-horizontal-panel! panel surface #f #f #f #f)
            (for-each (lambda (child)
                (soft-render-panel! child surface))
                (panel-children panel))))

    (define (render-horizontal-panel! panel surface x y w h)
        (panel-dirty?-set! panel #f)
        (if x
            (begin
                ; update dimensions of panel
                (panel-x-set! panel x)
                (panel-y-set! panel y)
                (panel-w-set! panel w)
                (panel-h-set! panel h))
            (begin
                ; use dimensions from last time
                (set! x (panel-x panel))
                (set! y (panel-y panel))
                (set! w (panel-w panel))
                (set! h (panel-h panel))))
        ; draw background
        (let ((back-rect (sdl2:make-rect x y w h)))
            (sdl2:fill-rect! surface back-rect background-color)
            (sdl2:free-rect! back-rect))
        ; render each child panel
        (let* ((count (length (panel-children panel)))
               (partitions (partition w count)))
            (render-panels-horizontally! (panel-children panel)
                partitions surface x y h)))

    (define (soft-render-panel! panel surface)
        (let ((type (panel-type panel)))
            (cond
                ((eq? type 'regular)
                    (soft-render-regular-panel! panel surface))
                ((eq? type 'horizontal)
                    (soft-render-horizontal-panel! panel surface)))))

    (define (render-panel! panel surface x y w h)
        (let ((type (panel-type panel)))
            (cond
                ((eq? type 'regular)
                    (render-regular-panel! panel surface
                        (+ x panel-margin)
                        (+ y panel-margin)
                        (- w (* panel-margin 2))
                        (- h (* panel-margin 2))))
                ((eq? type 'horizontal)
                    (render-horizontal-panel! panel surface x y w h)))))

    ; Event Processing

    (define-record-type mouse-down
        (make-mouse-down)
        mouse-down?)

    (define-record-type mouse-up
        (make-mouse-up)
        mouse-up?)

    (define-record-type mouse-move
        (make-mouse-move)
        mouse-move?)

    (define event-stream (stream:make))

    (define processed-events (stream:where (stream:select event-stream
        (lambda (event)
            (case (sdl2:event-type event)
                ((mouse-button-down)
                    (case (sdl2:mouse-button-event-button event)
                        ((left)
                            (make-mouse-down))
                        (else #f)))
                ((mouse-button-up)
                    (case (sdl2:mouse-button-event-button event)
                        ((left)
                            (make-mouse-up))
                        (else #f)))
                ((mouse-motion)
                    (make-mouse-move))
                (else #f))))
        (lambda (event) event))) ; only keep events that weren't mapped to #f

    (define mouse-events
        (stream:union
            (stream:where processed-events mouse-down?)
            (stream:where processed-events mouse-up?)
            (stream:where processed-events mouse-move?)))

    (define mouse-changes (stream:change mouse-events))

    (define clicks (stream:where mouse-changes
        (lambda (pair)
            (let ((current (car pair))
                  (previous (cdr pair)))
                (cond
                    ((not previous) #f)
                    ((not (mouse-up? current)) #f)
                    ((not (mouse-down? previous)) #f)
                    (else #t))))))

    (stream:sub! clicks (lambda (_)
        (print 'click)))

    ; Window

    (define (prune-panels!)
        (if root
            (set! root (prune-panel! root))))

    (define (rescan!)
        (if root
            (rescan-panel! root)))

    (define (redraw!)
        (let ((surface (sdl2:window-surface window)))
        (if (not root)
            (sdl2:fill-rect! surface #f background-color)
            (if (not (panel-dirty? root))
                (soft-render-panel! root surface)
                (if (eq? (panel-type root) 'regular)
                    (begin
                        (sdl2:fill-rect! surface #f background-color)
                        (render-regular-panel! root surface
                            panel-margin
                            panel-margin
                            (- (sdl2:surface-w surface) (* panel-margin 2))
                            (- (sdl2:surface-h surface) (* panel-margin 2))))
                    (render-panel! root surface
                        0
                        0
                        (sdl2:surface-w surface)
                        (sdl2:surface-h surface)))))
        (sdl2:update-window-surface! window)))

    (define (render!)
            (let ((surface (sdl2:window-surface window)))
                (sdl2:fill-rect! surface #f background-color)
                (if root
                    (render-panel! root surface
                        0
                        0
                        (sdl2:surface-w surface)
                        (sdl2:surface-h surface)))
                (sdl2:update-window-surface! window)))

    (define (add-panel! new-panel)
        (request-rescan)
        ; Add the new panel to the tree
        (cond
            ((not root)
                (set! root new-panel))
            ((eq? (panel-type root) 'regular)
                (set! root
                    (make-panel "combined" 'horizontal
                        (list root new-panel) 0 0 0 0 #f #t))
                (panel-dirty?-set! root #t))
            (else
                (panel-children-set! root
                    (append
                        (panel-children root)
                        (list new-panel)))
                (panel-dirty?-set! root #t))))

    (define (handle-events!)
        ; TODO use sdl2:event-state to turn off events I'm not listening for
        ; TODO sdl2:quit-requested?
        (sdl2:pump-events!)
        (let ((event (sdl2:make-event)))
            (while (sdl2:poll-event! event)
                (stream:send! event-stream event))
            (sdl2:free-event! event)))

    ;;;;;;;;;;; Exported Functions

    ; Core functions

    (define (init! window-name)
        (sdl2:set-main-ready!)
        (sdl2:init! '(video))
        (ttf:init!)
        (on-exit sdl2:quit!)
        (set! font (ttf:open-font "Aovel Sans Rounded.ttf" font-size))
        (set! window (sdl2:create-window! window-name 'undefined 'undefined' 500 400))
        '())

    (define (draw-loop! delay)
        (cond
            ; Make sure we have a window
            ((not window)
                (print "Mosaic has not been initialized"))
            (else
                (handle-events!)
                (if rescan-needed? 
                    (begin
                        (prune-panels!)
                        (rescan!)
                        (set! rescan-needed? #f)
                        (request-redraw)))
                (if redraw-needed?
                    (begin
                        (redraw!)
                        (set! redraw-needed? #f)))
                (sdl2:delay! delay))))

    (define (set-content! component . content)
        (cond
            ((buttons? component)
                (buttons-buttons-set! component
                    (button-definitions->button-list content)
                    (request-rescan)
                    #t))
            (else #f)))

    ; Panel Management

    (define (panel! name component)
        (let ((new-panel (make-panel name 'regular (list component) 0 0 0 0 #f #t)))
            (add-panel! new-panel)
            new-panel))

    ; Basic Components

    (define (buttons handler . button-definitions)
        (make-buttons handler
            (button-definitions->button-list button-definitions)
            0 0 0 0 #f #t))

    ; Helper macros
)
