#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
	     (ice-9 arrays)
	     (srfi srfi-1)
	     (srfi srfi-26))

(define (read-grid port)
  (let loop ((line (read-line port))
	     (elements '()))
    (if (eof-object? line)
	(list->array 2 (reverse elements))
	(loop (read-line port)
	      (cons (string->list line) elements)))))

(define guard-chars (string->char-set "^v<>"))

(define guard-dirs
  '((#\^ . (-1 0))
    (#\> . (0 1))
    (#\v . (1 0))
    (#\< . (0 -1))))

(define (find-guard grid)
  (define _ (array-copy grid))
  (call/cc
   (lambda (return)
     (array-index-map!
      _
      (lambda (row col)
	(when (char-set-contains?
	       guard-chars (array-ref grid row col))
	  (return `((,row ,col) . ,(array-ref grid row col)))))))))

(define (guard-loops? grid)
  (define guard (find-guard grid))
  (define guard-row (caar guard))
  (define guard-col (cadar guard))
  (define guard-dir (cdr guard))
  (define dims (array-dimensions grid))
  (define visited (make-array '() (car dims) (cadr dims)))
  (let loop ((row guard-row)
	     (col guard-col)
	     (dir guard-dir))
    (let* ((delta (assoc-ref guard-dirs dir))
	   (next-row (+ row (car delta)))
	   (next-col (+ col (cadr delta)))
	   (prev-state (array-ref visited row col)))
      (cond ((not (array-in-bounds? grid next-row next-col))
	     ;; If the guard leaves the grid, she does not loop
	     #f)
	    ((member dir prev-state)
	     ;; The guard has been here before, facing this direction-
	     ;; this grid loops
	     #t)
	    ((char=? #\# (array-ref grid next-row next-col))
	     (array-set! visited (cons dir prev-state) row col)
	     (loop
	      row col
	      (caadr (find-tail (compose (cute char=? dir <>) car)
				(apply circular-list guard-dirs)))))
	    (else
	     (array-set! visited (cons dir prev-state) row col)
	     (loop next-row next-col dir))))))

(define (loop-locations grid)
  "Given an input grid, return an array of the same dimensions where each
element of the array is #t if placing an obstacle there would result
in a loop, and #f otherwise."
  (define loop-locs (array-copy grid))
  (array-index-map!
   loop-locs
   (lambda (row col)
     (let ((curr-char (array-ref grid row col)))
       (if (char-set-contains? guard-chars curr-char)
	   ;; The guard is here, so skip this location
	   #f
	   (begin
	     (array-set! grid #\# row col)
	     (let ((loops? (guard-loops? grid)))
	       (array-set! grid curr-char row col)
	       loops?))))))
  loop-locs)

(define (array-count pred array)
  (count pred (fold append '() (array->list array))))

(define (main)
  (display (array-count
	    identity
	    (loop-locations (read-grid (current-input-port)))))
  (newline))

(main)
