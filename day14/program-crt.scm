#!/usr/bin/env guile
!#

(use-modules (ice-9 regex)
	     (ice-9 receive)
	     (ice-9 rdelim))

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

(define (get-robot-positions bounds robots seconds)
  (map (lambda (robot)
	 (simulate-robot bounds (car robot) (cadr robot) seconds))
       robots))

(define (mean lst)
  (/ (apply + lst) (length lst)))

(define (variance lst)
  (define lst-mean (mean lst))
  (mean (map (lambda (i) (expt (- i lst-mean) 2)) lst)))

(define (argmax lst less)
  (let loop ((idx 0)
	     (argmax 0)
	     (max #f)
	     (rest lst))
    (cond ((null? rest)
	   argmax)
	  ((not max)
	   (loop (1+ idx) idx (car rest) (cdr rest)))
	  ((less (car rest) max)
	   (loop (1+ idx) argmax max (cdr rest)))
	  (else
	   (loop (1+ idx) idx (car rest) (cdr rest))))))

(define (most-clustered-second bounds robots)
  "Adapted to Scheme from
https://www.reddit.com/r/adventofcode/comments/1he0asr/comment/m1zzfsh/
and
https://www.reddit.com/r/adventofcode/comments/1hdvhvu/comment/m1zws1g/"
  (define x-variances
    (map (lambda (sec)
	   (variance
	    (map car (get-robot-positions bounds robots sec))))
	 (iota (car bounds))))
  (define y-variances
    (map (lambda (sec)
	   (variance
	    (map cadr (get-robot-positions bounds robots sec))))
	 (iota (cadr bounds))))
  (define x-min-var (argmax x-variances >))
  (define y-min-var (argmax y-variances >))
  (define inverse-w (modulo-expt (car bounds) -1 (cadr bounds)))
  (+ x-min-var (* (modulo (* inverse-w (- y-min-var x-min-var))
			  (cadr bounds))
		  (car bounds))))

(define (main)
  (define robots (read-robots (current-input-port)))
  (display (most-clustered-second '(101 103) robots))
  (newline))

(main)
