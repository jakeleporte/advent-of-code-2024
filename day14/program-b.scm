#!/usr/bin/env guile
!#

(use-modules (ice-9 match)
	     (ice-9 regex)
	     (ice-9 receive)
	     (ice-9 rdelim)
	     (ice-9 textual-ports)
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

(define (number-sign number)
  (cond ((positive? number) 1)
	((zero? number) 0)
	((negative? number) -1)))

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

(define (display-robot-grid grid)
  (for-each
   (lambda (row)
     (display (list->string row))
     (newline))
   (array->list grid)))

(define (run? row char run-length)
  (let find-loop ((rest row))
    (if (null? rest)
	#f
	(let ((first-char (drop-while (lambda (c) (not (char=? c char)))
				   rest)))
	  (receive (chars tail)
	      (span (lambda (c) (char=? c char)) first-char)
	    (if (>= (length chars) run-length)
		#t
		(find-loop tail)))))))

(define (main)
  (define robots (call-with-input-file "input" read-robots))
  (let loop ((secs 0))
    (when (<= secs 10000)
      (let ((grid (get-robot-grid '(101 103) robots secs)))
	(when (any (lambda (row) (run? row #\# 10)) (array->list grid))
	  (display secs) (newline)
	  (display-robot-grid grid)))
      (loop (1+ secs)))))

(main)
