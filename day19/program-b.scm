#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
	     (srfi srfi-26))

(define (read-towels port)
  (string-tokenize (read-line port) char-set:letter))

(define (memoized-procedure proc)
  (define cache (make-hash-table))
  (lambda args
    (or (hash-ref cache (cdr args))
	(let ((val (apply proc args)))
	  (hash-set! cache (cdr args) val)))))

(define (design-options towels design)
  (define prefixes (filter (cute string-prefix? <> design) towels))
  (define remaining (map (lambda (s) (string-drop design (string-length s)))
			 prefixes))
  (if (string-null? design)
      1
      (apply
       +
       (map (cute design-options towels <>) remaining))))

(set! design-options (memoized-procedure design-options))

(define (count-towel-matches towels port)
  (let loop ((line (read-line port))
	     (total-matched 0))
    (if (eof-object? line)
	total-matched
	(loop (read-line port) (+ total-matched (design-options towels line))))))

(define (main)
  (define towels (read-towels (current-input-port)))
  (read-line (current-input-port))
  (display (count-towel-matches towels (current-input-port)))
  (newline))

(main)
