#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
	     (srfi srfi-26))

(define (read-page-rule dependencies line)
  "Read page rule defined by LINE and store the resulting rule in the
hash table DEPENDENCIES."
  (let* ((page-nums (map string->number (string-split line #\|)))
	 (page-1 (car page-nums))
	 (page-2 (cadr page-nums)))
    (hashv-set! dependencies
		page-2
		(cons page-1 (hashv-ref dependencies page-2 '())))))

(define (read-page-rules port)
  "Read a list of page rules from the input port until a blank line is
found."
  (define dependencies (make-hash-table))
  (let loop ((line (read-line port)))
    (if (or (eof-object? line)
	    (string-null? line))
	dependencies
	(begin
	  (read-page-rule dependencies line)
	  (loop (read-line))))))

(define (read-page-list line)
  (map string->number (string-split line #\,)))

(define (page-less dependencies page-1 page-2)
  (let ((deps (hashv-ref dependencies page-2 '())))
    (member page-1 deps)))

(define (page-list-valid? dependencies page-list)
  (sorted? page-list (cute page-less dependencies <> <>)))

(define (process-lines dependencies port)
  (let loop ((line (read-line port))
	     (total 0))
    (if (eof-object? line)
	total
	(let ((page-list (read-page-list line)))
	  (if (page-list-valid? dependencies page-list)
	      (loop (read-line port) total)
	      (loop (read-line port)
		    (+ total
		       (list-ref (sort page-list
				       (cute page-less dependencies <> <>))
				 (floor/ (length page-list) 2)))))))))

(define (main)
  (let ((dependencies (read-page-rules (current-input-port))))
    (display (process-lines dependencies (current-input-port)))
    (newline)))

(main)
