#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
	     (srfi srfi-1)
	     (srfi srfi-26)
	     (srfi srfi-43))

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

(define (count-groups val eq lst)
  (let loop ((rest lst)
	     (total 0)
	     (previous-eq? #f))
    (cond ((null? rest) total)
	  ((and (not previous-eq?)
		(eq val (car rest)))
	   (loop (cdr rest) (1+ total) #t))
	  ((not (eq val (car rest)))
	   (loop (cdr rest) total #f))
	  (else
	   (loop (cdr rest) total previous-eq?)))))

(define (region-horizontal-edges grid region)
  (define dims (array-dimensions grid))
  (define row-count (car dims))
  (define col-count (cadr dims))
  (define prev-row (make-vector col-count #f))
  (define bool-grid (make-array #f row-count col-count))
  (array-index-map! bool-grid (lambda (i j) (hash-ref region `(,i ,j))))
  ;; Sweep a horizontal line from the top of the grid to the bottom,
  ;; counting how many times each element of the line enters or exits
  ;; a region
  (let loop ((row-idx 0)
	     (edge-count 0))
    (if (> row-idx row-count)
	edge-count
	(let* ((curr-row (if (= row-idx row-count)
			     (make-vector col-count #f)
			     (array-slice bool-grid row-idx)))
	       (diff (make-vector col-count #f)))
	  (array-map!
	   diff
	   (lambda (c p) (cond ((eq? c p) 'same)
			  ((and (not p) c) 'up)
			  ((and p (not c) 'down))))
	   curr-row prev-row)
	  (set! prev-row curr-row)
	  (loop (1+ row-idx) (+ edge-count
				(count-groups 'up eq? (vector->list diff))
				(count-groups 'down eq? (vector->list diff))))))))

(define (region-edges grid region)
  (+ (region-horizontal-edges grid region)
     (region-horizontal-edges (transpose-array grid 1 0) region)))

(define (region-cost grid region)
  (* (region-area region) (region-edges grid region)))

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

(define (show-sorted-edges grid region)
  (define edges (region-edges grid region))
  (define dims (array-dimensions grid))
  (define new-grid (apply make-array 1 dims))
  (for-each
   (lambda (edge-pos)
     (apply array-set! new-grid 8 edge-pos))
   edges)
  new-grid)

(define (main)
  (let* ((grid (read-grid (current-input-port)))
	 (regions (find-regions grid)))
    (display (regions-cost grid regions))
    (newline)))

(main)
