#!/usr/bin/env guile
!#

(eval-when (expand load eval)
  (define parent-directory
    (string-join
     (reverse
      (cdr
       (reverse
	(string-split (dirname (current-filename))
		      (string->char-set file-name-separator-string)))))
     file-name-separator-string)))

(add-to-load-path parent-directory)

(use-modules (ice-9 rdelim)
	     (srfi srfi-26)
	     (scram function))

(define (read-towels port)
  (string-tokenize (read-line port) char-set:letter))

(define-memoized (design-options towels design)
  (define prefixes (filter (cute string-prefix? <> design) towels))
  (define remaining (map (lambda (s) (string-drop design (string-length s)))
			 prefixes))
  (if (string-null? design)
      1
      (apply
       +
       (map (cute design-options towels <>) remaining))))

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
