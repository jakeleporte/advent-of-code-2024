#!/usr/bin/env guile
!#

(use-modules (ice-9 regex)
	     (ice-9 rdelim)
	     (srfi srfi-26))

(define (read-towels port)
  (string-tokenize (read-line port) char-set:letter))

(define (count-towel-matches towels port)
  (define towel-patt
    (string-join (map (cute string-append "(" <> ")") towels)
		 "|"))
  (define design-patt
    (string-append "^(" towel-patt ")+$"))
  (define design-rx (make-regexp design-patt))
  (let loop ((line (read-line port))
	     (total-matched 0))
    (if (eof-object? line)
	total-matched
	(loop (read-line port)
	      (+ total-matched (if (regexp-exec design-rx line) 1 0))))))

(define (main)
  (define towels (read-towels (current-input-port)))
  (read-line (current-input-port))
  (display (count-towel-matches towels (current-input-port)))
  (newline))

(main)
