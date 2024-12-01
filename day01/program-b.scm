#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
	     (srfi srfi-1)
	     (srfi srfi-26))

(define (insert-all-sorted numbers new-numbers less)
  (if (null? numbers) (map list new-numbers)
      (map (cute merge <> <> less)
	   numbers
	   (map list new-numbers))))

(define (read-number-lists port)
  (let loop ((line (read-line port))
	     (numbers '()))
    (if (eof-object? line)
	numbers
	(loop (read-line port)
	      (insert-all-sorted
	       numbers
	       (map string->number
		    (string-tokenize line)) <)))))

(define (get-similarity-score numbers)
  (fold (lambda (e prev)
	  (+ prev
	     (* e (count (cut = e <>)
			 (apply append (cdr numbers))))))
	0 (car numbers)))

(define (main)
  (display
   (get-similarity-score
    (read-number-lists (current-input-port))))
  (newline))

(main)
