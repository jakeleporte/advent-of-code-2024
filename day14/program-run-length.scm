#!/usr/bin/env guile
!#

(use-modules (ice-9 regex)
	     (ice-9 receive)
	     (ice-9 rdelim)
	     (srfi srfi-1))

(define
  robot-regexp
  (make-regexp
   (string-append "p=([[:digit:]]+),([[:digit:]]+)"
		  " v=(-?[[:digit:]]+),(-?[[:digit:]]+)$")))

(define (read-robot-line line)
  (let ((m (regexp-exec robot-regexp line)))
   (values (map string->number (list (match:substring m 1)
				     (match:substring m 2)))
	   (map string->number (list (match:substring m 3)
				     (match:substring m 4))))))

(define (simulate-robot bounds position velocity seconds)
  (let* ((total-delta (map (lambda (v) (* v seconds)) velocity))
	 (unwrapped-pos (map + position total-delta)))
    (map modulo unwrapped-pos bounds)))

(define (read-robots port)
  (let loop ((line (read-line port))
	     (robots '()))
    (if (eof-object? line)
	robots
	(receive (position velocity)
	    (read-robot-line line)
	  (loop (read-line port)
		(cons `(,position ,velocity) robots))))))

(define (get-robot-grid bounds robots seconds)
  (define positions
    (map (lambda (robot)
	   (simulate-robot bounds (car robot) (cadr robot) seconds))
	 robots))
  (define grid (make-array #\space (cadr bounds) (car bounds)))
  (for-each
   (lambda (pos) (array-set! grid #\# (cadr pos) (car pos)))
   positions)
  grid)

(define (run-length-complexity lst)
  (let loop ((curr-e #f)
	     (complexity 0)
	     (rest lst))
    (cond ((null? rest)
	   complexity)
	  ((or (not curr-e) (not (eqv? curr-e (car rest))))
	   (loop (car rest) (1+ complexity) (cdr rest)))
	  (else
	   (loop curr-e complexity (cdr rest))))))

(define (find-min-complexity-frame robots bounds)
  (let loop ((sec 0)
	     (argmin 0)
	     (min-complexity #f))
    (let* ((grid (get-robot-grid bounds robots sec))
	   (flat-grid (append-map identity (array->list grid)))
	   (new-complexity (run-length-complexity flat-grid)))
     (cond ((= sec (apply * bounds))
	    argmin)
	   ((or (not min-complexity) (< new-complexity min-complexity))
	    (loop (1+ sec) sec new-complexity))
	   (else
	    (loop (1+ sec) argmin min-complexity))))))

(define (main)
  (define robots (read-robots (current-input-port)))
  (display (find-min-complexity-frame robots '(101 103)))
  (newline))

(main)
