#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
	     (srfi srfi-1)
	     (srfi srfi-26))

(define (difference-list lst)
  (reverse
   (fold (lambda (e p l) (cons (- e p) l))
	 '() (cdr lst) lst)))

(define (report-safe? rpt)
  (let ((dl (difference-list rpt)))
    (and (every (compose (cute <= <> 3) abs) dl)
	 (or (every positive? dl)
	     (every negative? dl)))))

(define (count-reports pred port)
  (let loop ((line (read-line))
	     (count 0))
    (if (eof-object? line) count
	(loop (read-line)
	      (if (pred (map string->number (string-tokenize line)))
		  (1+ count)
		  count)))))

(define (main)
  (display (count-reports report-safe? (current-input-port)))
  (newline))

(main)
