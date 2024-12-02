#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
	     (srfi srfi-1)
	     (srfi srfi-26))

(define (difference-list lst)
  (reverse
   (fold (lambda (e p l) (cons (- e p) l))
	 '() (cdr lst) lst)))

(define (diff-valid? parity d)
  (and (parity d) (and (<= (abs d) 3) (>= (abs d) 1))))

(define (report-safe? rpt)
  (let ((dl (difference-list rpt)))
    (and (every (compose (cute <= <> 3) abs) dl)
	 (or (every positive? dl)
	     (every negative? dl)))))

(define (report-almost-safe? rpt)
  (if (report-safe? rpt) #t
      (let* ((dl (difference-list rpt))
	     (count+ (count positive? dl))
	     (count- (count negative? dl))
	     (parity (if (> count+ count-) positive? negative?))
	     (valid? (cute diff-valid? parity <>))
	     (first-p (list-index (negate valid?) dl))
	     (valid-r (take rpt (1+ first-p)))
	     (rest-r (drop rpt (1+ first-p))))
	(or (report-safe? (append (drop-right valid-r 1) rest-r))
	    (report-safe? (append valid-r (drop rest-r 1)))))))

(define (count-reports pred port)
  (let loop ((line (read-line))
	     (count 0))
    (if (eof-object? line) count
	(loop (read-line)
	      (if (pred (map string->number (string-tokenize line)))
		  (1+ count)
		  count)))))

(define (main)
  (display (count-reports report-almost-safe? (current-input-port)))
  (newline))

(main)
