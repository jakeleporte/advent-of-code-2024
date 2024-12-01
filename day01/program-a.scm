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

(define (get-total-difference numbers)
  (reduce + 0 (apply (cute map (compose abs -) <...>) numbers)))

(define (main)
  (display
   (get-total-difference
    (read-number-lists (current-input-port))))
  (newline))

(main)
