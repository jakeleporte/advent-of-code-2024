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

(define (find-guard map)
  (define guard-chars (string->char-set "^v<>"))
  (define _ (array-copy map))
  (call/cc
   (lambda (return)
     (array-index-map!
      _
      (lambda (row col)
	(when (char-set-contains?
	       guard-chars (array-ref map row col))
	  (return `((,row ,col) . ,(array-ref map row col)))))))))

(define guard-dirs
  '((#\^ . (-1 0))
    (#\> . (0 1))
    (#\v . (1 0))
    (#\< . (0 -1))))

(define (simulate-guard map)
  (define guard (find-guard map))
  (define guard-row (caar guard))
  (define guard-col (cadar guard))
  (define guard-dir (cdr guard))
  (let loop ((row guard-row)
	     (col guard-col)
	     (dir guard-dir))
    (let* ((delta (assoc-ref guard-dirs dir))
	   (next-row (+ row (car delta)))
	   (next-col (+ col (cadr delta))))
      ;; Mark the now visited spot
      (array-set! map #\X row col)
      ;; Return the map if the guard will go out of bounds next round
      (cond ((not (array-in-bounds? map next-row next-col))
	     map)
	    ((char=? #\# (array-ref map next-row next-col))
	     (loop
	      row col
	      (caadr (find-tail (compose (cute char=? dir <>) car)
				(apply circular-list guard-dirs)))))
	    (else (loop next-row next-col dir))))))

(define (count-xs map)
  (count (cute char=? #\X <>) (fold append '() (array->list map))))

(define (main)
  (display (count-xs (simulate-guard (read-grid (current-input-port)))))
  (newline))

(main)
