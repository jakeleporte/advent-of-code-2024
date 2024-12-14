#!/usr/bin/env guile
!#

(use-modules (ice-9 match)
	     (ice-9 regex)
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

(define (number-sign number)
  (cond ((positive? number) 1)
	((zero? number) 0)
	((negative? number) -1)))

(define (quadrant bounds position)
  (define middle
    (map (lambda (bound) (floor-quotient bound 2)) bounds))
  (define quadrant-coords
    (map number-sign (map - position middle)))
  (match quadrant-coords
    ((or `(0 ,_) `(,_ 0)) 0)
    ('(-1 -1) 1)
    ('(1 -1) 2)
    ('(-1 1) 3)
    ('(1 1) 4)
    (_ (error "invalid quadrant"))))

(define (count-robot-quadrants bounds seconds port)
  (define quadrant-counts (make-vector 5 0))
  (let loop ((line (read-line port)))
    (if (eof-object? line)
	(drop (vector->list quadrant-counts) 1)
	(receive (position velocity)
	    (read-robot-line line)
	  (let* ((new-pos (simulate-robot bounds position velocity seconds))
		 (quad (quadrant bounds new-pos))
		 (prev (vector-ref quadrant-counts quad)))
	    (vector-set! quadrant-counts quad (1+ prev))
	    (loop (read-line port)))))))

(define (main)
  (let ((counts (count-robot-quadrants
		 '(101 103) 100 (current-input-port))))
    (display (apply * counts)) (newline)))

(main)
