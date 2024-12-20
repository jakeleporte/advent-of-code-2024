(define-module (scram function)
  #:export (memoized-procedure
	    define-memoized))

(define (memoized-procedure proc)
  (define cache (make-hash-table))
  (lambda args
    (or (hash-ref cache (cdr args))
	(let ((val (apply proc args)))
	  (hash-set! cache (cdr args) val)))))

(define-syntax-rule (define-memoized (name . formals) body body* ...)
  (begin
    (define (name . formals) body body* ...)
    (set! name (memoized-procedure name))))
