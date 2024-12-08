#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
	     (srfi srfi-1)
	     (srfi srfi-26))

(define (hashv-append! table key val)
  (hashv-set! table key
	      (cons val (hashv-ref table key '()))))

(define (read-grid port)
  (let loop ((line (read-line port))
	     (rows '()))
    (if (eof-object? line)
	(list->array 2 (reverse rows))
	(loop (read-line port) (cons (string->list line) rows)))))

(define (get-antenna-locations grid)
  (define rows (array->list grid))
  (define antenna-locations (make-hash-table))
  (let row-loop ((rest-rows rows)
		 (row-idx 0))
    (if (null? rest-rows)
	antenna-locations
	(let col-loop ((chars (car rest-rows))
		       (col-idx 0))
	  (if (null? chars)
	      (row-loop (cdr rest-rows) (1+ row-idx))
	      (begin
		(when (char-set-contains? char-set:letter+digit (car chars))
		  (hashv-append!
		   antenna-locations (car chars) (list row-idx col-idx)))
		(col-loop (cdr chars) (1+ col-idx))))))))

(define (mark-harmonic-antinodes! grid a b)
  (define slope (map - a b))
  (let loop ((location (map - a slope)))
    (when (apply array-in-bounds? grid location)
      (apply array-set! grid #\# location)
      (loop (map - location slope))))
  (let loop ((location (map + b slope)))
    (when (apply array-in-bounds? grid location)
      (apply array-set! grid #\# location)
      (loop (map + location slope)))))

(define (choose-2 lst)
  (fold
   (lambda (i prev)
     (append
      prev
      (map (lambda (j) (list i j))
	   (cdr (find-tail (cute equal? i <>) lst)))))
   '()
   lst))

(define (mark-all-antinodes! grid)
  (define antenna-locations (get-antenna-locations grid))
  (hash-for-each
   (lambda (key antennas)
     (for-each
      (lambda (antenna-pair)
	(apply mark-harmonic-antinodes! grid antenna-pair))
      (choose-2 antennas)))
   antenna-locations))

(define (count-antinodes grid)
  (count (cute eqv? #\# <>) (append-map identity (array->list grid))))

(define (main)
  (let ((grid (read-grid (current-input-port))))
    (mark-all-antinodes! grid)
    (display (count-antinodes grid))
    (newline)))

(main)
