(module streams
    (make send! sub! unsub!
     union where select change pair)

    (import
        scheme
        (chicken base))

    ; util

    (define (filter pred lst)
        (cond
            ((null? lst) '())
            ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
            (else (filter pred (cdr lst)))))

    ; data type

    (define-record-type stream
        (make-stream callbacks)
        stream?
        (callbacks stream-callbacks set-stream-callbacks!))

    ; exported stream functions

    (define (make)
        (make-stream '()))

    (define (send! stream value)
        (assert (stream? stream))
        (for-each (lambda (callback)
                (callback value))
            (stream-callbacks stream)))

    (define (sub! stream callback)
        (assert (stream? stream))
        (assert (procedure? callback))
        (set-stream-callbacks! stream
            (cons callback (stream-callbacks stream))))

    (define (unsub! stream callback)
        (assert (stream? stream))
        (set-stream-callbacks! stream
            (filter (lambda (cb) (not (eq? callback cb))))))

    ; Helper functions

    (define (union . streams)
        (for-each (lambda (stream) (assert (stream? stream))) streams)
        (define our-stream (make))
        (define (update val)
            (send! our-stream val))
        (for-each (lambda (stream) (sub! stream update)) streams)
        our-stream)

    (define (where stream pred)
        (assert (stream? stream))
        (define our-stream (make))
        (define (update val)
            (if (pred val) (send! our-stream val)))
        (sub! stream update)
        our-stream)

    (define (select stream mapper)
        (assert (stream? stream))
        (assert (procedure? mapper))
        (define our-stream (make))
        (define (update val)
            (send! our-stream (mapper val)))
        (sub! stream update)
        our-stream)

    (define (change stream)
        (assert (stream? stream))
        (define our-stream (make))
        (define previous-val #f)
        (define (update val)
            (send! our-stream (cons val previous-val))
            (set! previous-val val))
        (sub! stream update)
        our-stream)

    (define (pair stream1 stream2)
        (assert (stream? stream1))
        (assert (stream? stream2))
        (define a #f)
        (define b #f)
        (define out-stream (make))
        (sub! stream1 (lambda (val)
            (set! a val)
            (send! out-stream (cons a b))))
        (sub! stream2 (lambda (val)
            (set! b val)
            (send! out-stream (cons a b))))
        out-stream)
)
