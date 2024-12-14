#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
	     (ice-9 regex)
	     (srfi srfi-1)
	     (srfi srfi-26))

(define button-regexp
  (make-regexp "^Button [AB]: X\\+([[:digit:]]+), Y\\+([[:digit:]]+)$"))

(define prize-regexp
  (make-regexp "^Prize: X=([[:digit:]]+), Y=([[:digit:]]+)$"))

(define (safe-read-line port)
  (define line (read-line port))
  (if (eof-object? line)
      ""
      line))

(define (read-claw-spec port)
  (let* ((button-a (regexp-exec button-regexp (safe-read-line port)))
	 (button-b (regexp-exec button-regexp (safe-read-line port)))
	 (prize (regexp-exec prize-regexp (safe-read-line port))))
    (if (not (and button-a button-b prize))
	#f
	(let ((claw-spec (map (lambda (m) (map (cute match:substring m <>) '(1 2)))
			      (list button-a button-b prize))))
	  (transpose-array
	   (list->array
	    2 (map (lambda (row) (map string->number row)) claw-spec))
	   1 0)))))

(define (matrix-column arr col)
  "Return the column at index COL in 2-dimensional array ARR as a vector."
  (define row-count (car (array-dimensions arr)))
  (list->vector
   (array->list
    (make-shared-array arr (lambda (i) (list i col)) row-count))))

(define (matrix-row arr row)
  (list->vector
   (array->list
    (array-cell-ref arr row))))

(define (vector-argmax vec less)
  (define len (vector-length vec))
  (let loop ((idx 0)
	     (argmax 0))
    (cond ((= idx len)
	   argmax)
	  ((less (vector-ref vec idx)
		 (vector-ref vec argmax))
	   (loop (1+ idx) argmax))
	  (else
	   (loop (1+ idx) idx)))))

(define (abs-less a b)
  (< (abs a) (abs b)))

(define (matrix-swap-rows! arr idx-a idx-b)
  (define col-count (cadr (array-dimensions arr)))
  (define row-a (make-array 0 col-count))
  (array-copy! (array-cell-ref arr idx-a) row-a)
  (let ((row-b (array-cell-ref arr idx-b)))
    (array-cell-set! arr row-b idx-a)
    (array-cell-set! arr row-a idx-b)))

(define (matrix-row-multiply! arr row-idx factor)
  (define row (array-cell-ref arr row-idx))
  (array-map! row (cute * factor <>) row))

(define (matrix-row-echelon! arr)
  "Algorithm taken from pseudocode found at
https://en.wikipedia.org/wiki/Gaussian_elimination#Pseudocode"
  (define dims (array-dimensions arr))
  (define row-count (car dims))
  (define col-count (cadr dims))
  (let loop ((pivot-row 0)
	     (pivot-col 0))
    (when (and (< pivot-row row-count) (< pivot-col col-count))
      (let* ((column-k (matrix-column arr pivot-col))
	     (pivot-i (vector-argmax column-k abs-less)))
	(if (= 0 (vector-ref column-k pivot-i))
	    (loop pivot-row (1+ pivot-col))
	    (begin
	      ;; (matrix-swap-rows! arr pivot-row pivot-i)
	      (let row-loop ((row-i (1+ pivot-row)))
		(when (< row-i row-count)
		  (let ((f (/ (array-ref arr row-i pivot-col)
			      (array-ref arr pivot-row pivot-col))))
		    (array-set! arr 0 row-i pivot-col)
		    (let col-loop ((col-j (1+ pivot-col)))
		      (when (< col-j col-count)
			(array-set!
			 arr (- (array-ref arr row-i col-j)
				(* f (array-ref arr pivot-row col-j)))
			 row-i col-j)
			(col-loop (1+ col-j)))))
		  (row-loop (1+ row-i))))
	      (loop (1+ pivot-row) (1+ pivot-col))))))))

(define (matrix-reduced-row-echelon! arr)
  (define dims (array-dimensions arr))
  (define row-count (car dims))
  (define col-count (cadr dims))
  (matrix-row-echelon! arr)
  (let loop ((pivot-row 0)
	     (pivot-col 0))
    (when (and (< pivot-row row-count) (< pivot-col col-count))
      (if (= 0 (array-ref arr pivot-row pivot-col))
	  (loop pivot-row (1+ pivot-col))
	  (begin
	    (matrix-row-multiply! arr pivot-row
				 (/ (array-ref arr pivot-row pivot-col)))
	    (let row-loop ((row-i (1- pivot-row)))
	      (when (>= row-i 0)
		(let ((f (/ (array-ref arr row-i pivot-col)
			    (array-ref arr pivot-row pivot-col))))
		  (array-set! arr 0 row-i pivot-col)
		  (let col-loop ((col-j (1+ pivot-col)))
		    (when (< col-j col-count)
		      (array-set!
		       arr (- (array-ref arr row-i col-j)
			      (* f (array-ref arr pivot-row col-j)))
		       row-i col-j)
		      (col-loop (1+ col-j)))))
		(row-loop (1- row-i))))
	    (loop (1+ pivot-row) pivot-col))))))

(define (matrix-identity n)
  (define arr (make-array 0 n n))
  (array-index-map! arr (lambda (i j) (if (= i j) 1 0)))
  arr)

(define (claw-spec-tokens claw-spec a-cost b-cost)
  (define dims (array-dimensions claw-spec))
  (define solution (apply make-array 0 dims))
  (define id (matrix-identity 2))
  (array-copy! claw-spec solution)
  (matrix-reduced-row-echelon! solution)
  (let ((factors (make-shared-array solution list 2 2))
	(result (vector->list (matrix-column solution 2))))
    (if (and (array-equal? factors id)
	     (every integer? result)
	     (every positive? result))
	(+ (* a-cost (first result))
	   (* b-cost (second result)))
	0)))

(define (sum-claw-tokens port)
  (let loop ((claw-spec (read-claw-spec port))
	     (total 0))
    (read-line port)
    (if (not claw-spec)
	total
	(loop (read-claw-spec port)
	      (+ total (claw-spec-tokens claw-spec 3 1))))))

(define (main)
  (display (sum-claw-tokens (current-input-port)))
  (newline))

(main)
