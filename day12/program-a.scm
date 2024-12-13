#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
	     (srfi srfi-1)
	     (srfi srfi-26))

(define (read-grid port)
  (let loop ((line (read-line port))
	     (rows '()))
    (if (eof-object? line)
	(list->array 2 (reverse rows))
	(loop (read-line port)
	      (cons (string->list line)
		    rows)))))

(define dirs
  '((1 0) (0 1) (-1 0) (0 -1)))

(define (get-neighbor-coords grid pos)
  (filter (cute apply array-in-bounds? grid <>)
	  (map (cute map + pos <>) dirs)))

(define (get-matching-neighbor-coords grid char pos)
  (filter (lambda (neighbor-coord)
	    (char=? char (apply array-ref grid neighbor-coord)))
	  (get-neighbor-coords grid pos)))

(define (get-new-fringe grid char region fringe)
  (define new-fringe '())
  (for-each
   (lambda (fringe-coord)
     (for-each
      (lambda (neighbor)
	(when (not (hash-ref region neighbor))
	  (set! new-fringe (lset-adjoin equal? new-fringe neighbor))))
      (get-matching-neighbor-coords grid char fringe-coord)))
   fringe)
  new-fringe)

(define (character-fence grid position)
  (define character (apply array-ref grid position))
  (- 4
     (count
      identity
      (map (lambda (new-pos)
	     (char=? character
		     (apply array-ref grid new-pos)))
	   (get-neighbor-coords grid position)))))

(define (find-region grid start-position)
  (define region (make-hash-table))
  (define character (apply array-ref grid start-position))
  (let loop ((fringe (list start-position)))
    (if (null? fringe)
	region
	(begin
	  (for-each (lambda (pos) (hash-set! region pos #t)) fringe)
	  (loop (get-new-fringe grid character region fringe))))))

(define (region-area region)
  (hash-count (const #t) region))

(define (region-perimeter grid region)
  (hash-fold (lambda (key value prior)
		(+ prior (character-fence grid key)))
	     0
	     region))

(define (region-cost grid region)
  (* (region-area region) (region-perimeter grid region)))

(define (find-regions grid)
  (define cost 0)
  (define dims (array-dimensions grid))
  (define positions
    (append-map (lambda (row) (map (lambda (col) `(,row ,col)) (iota (cadr dims))))
		(iota (car dims))))
  (define remaining (make-hash-table))
  (for-each
   (lambda (pos)
     (hash-set! remaining pos #t))
   positions)
  (let loop ((rest positions)
	     (regions '()))
    (cond ((null? rest)
	   regions)
	  ((not (hash-ref remaining (car rest)))
	   (loop (cdr rest) regions))
	  (else
	   (let ((new-region (find-region grid (car rest))))
	     (hash-for-each
	      (lambda (key value) (hash-set! remaining key #f))
	      new-region)
	     (loop (cdr positions) (cons new-region regions)))))))

(define (regions-cost grid regions)
  (let loop ((rest regions)
	     (total 0))
    (if (null? rest)
	total
	(loop (cdr rest) (+ total (region-cost grid (car rest)))))))

(define (main)
  (let* ((grid (read-grid (current-input-port)))
	 (regions (find-regions grid)))
    (display (regions-cost grid regions))
    (newline)))

(main)
