#!/usr/bin/env guile
!#

(use-modules (ice-9 match)
	     (ice-9 rdelim)
	     (srfi srfi-1)
	     (srfi srfi-26))

(define (read-digit-grid port)
  (let loop ((line (read-line port))
	     (rows '()))
    (if (eof-object? line)
	(list->array 2 (reverse rows))
	(loop (read-line port)
	      (cons (map (compose string->number string)
			 (string->list line))
		    rows)))))

(define (move-grid direction position)
  (map + position
       (match direction
	 ('up '(-1 0))
	 ('down '(1 0))
	 ('left '(0 -1))
	 ('right '(0 1))
	 (_ (error "direction must be up, down, left, or right")))))

(define (_trailhead-score grid visited start-value position)
  (define curr-value (and (apply array-in-bounds? grid position)
			  (apply array-ref grid position)))
  (cond ((or (> start-value 9) (< start-value 0))
	 (error "start-value must be between 0 and 9 inclusive"))
	((or (not curr-value)
	     (not (= curr-value start-value))
	     (apply array-ref visited position))
	 0)
	((= curr-value 9)		;start-value is also 9
	 (apply array-set! visited #t position)
	 1)
	(else
	 (apply + (map (cute _trailhead-score grid visited (1+ start-value) <>)
		       (map (cute move-grid <> position)
			    '(up down left right)))))))

(define (trailhead-score grid start-value position)
  (define grid-dimensions (array-dimensions grid))
  (define visited (apply make-array #f grid-dimensions))
  (_trailhead-score grid visited start-value position))

(define (all-trailhead-scores grid)
  (define total 0)
  (define grid-dimensions (array-dimensions grid))
  (define scores (apply make-array #f grid-dimensions))
  (array-copy! grid scores)
  (array-index-map!
   scores
   (lambda (row-idx col-idx)
     (trailhead-score grid 0 `(,row-idx ,col-idx))))
  (apply + (append-map identity (array->list scores))))

(define (main)
  (display (all-trailhead-scores (read-digit-grid (current-input-port))))
  (newline))

(main)
