(require racket/control)

(define evidence (hash))

(define (handler l h v)
    (cobble-prompt (make-continuation-prompt-tag) l h v)
)

(define (cobble-prompt m l h v)
    (let* [
            (old-evidence evidence)
        ]
        (set! evidence (hash-set evidence l (list m h old-evidence)))
        (begin0 
            (prompt0-at m (v))
            (set! evidence old-evidence)
        ))
)

(define (perform l op argument)
    (let* [
        (perform-evidence evidence)
        (ev (hash-ref evidence l))
        (m (car ev))
        (h (cadr ev))
        (f (hash-ref h op))
    ]
    ; the semantics really *look like* those of shift0-at, but cobble-prompt also sets the evidence
    ; vector, so we have to add the prompt manually
    (control0-at m k (f argument (lambda (x) (cobble-prompt m l h (lambda () (k x))))))
))

