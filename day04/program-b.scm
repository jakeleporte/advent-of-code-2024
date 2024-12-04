#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
	     (srfi srfi-1)
	     (srfi srfi-26))

(define (read-wordsearch port)
  (let loop ((line (read-line port))
	     (elements '()))
    (if (eof-object? line)
	(list->array 2 (reverse elements))
	(loop (read-line port)
	      (cons (string->list line) elements)))))

(define (search-x-mas ws row-idx col-idx)
  (define dims (array-dimensions ws))
  (define in-bounds
    (every (lambda (d b) (and (> d 0) (< d (1- b))))
	   (list row-idx col-idx) dims))
  (cond
   ((not in-bounds) #f)
   ((not (char=? (array-ref ws row-idx col-idx) #\A)) #f)
   (else (let ((tl (array-ref ws (1- row-idx) (1- col-idx)))
	       (tr (array-ref ws (1- row-idx) (1+ col-idx)))
	       (bl (array-ref ws (1+ row-idx) (1- col-idx)))
	       (br (array-ref ws (1+ row-idx) (1+ col-idx))))
	   (and (or (and (char=? tl #\M)
			 (char=? br #\S))
		    (and (char=? tl #\S)
			 (char=? br #\M)))
		(or (and (char=? tr #\M)
			 (char=? bl #\S))
		    (and (char=? tr #\S)
			 (char=? bl #\M))))))))

(define (count-x-mas ws)
  (define dims (array-dimensions ws))
  (define count-array (make-array 0 (car dims) (cadr dims)))
  (array-index-map! count-array (cute search-x-mas ws <> <>))
  (count identity (fold append '() (array->list count-array))))

(define (main)
  (display (count-x-mas (read-wordsearch (current-input-port))))
  (newline))

(main)
