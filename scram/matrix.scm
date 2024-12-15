(define-module (scram matrix)
  #:export (matrix?
	    matrix-column
	    matrix-row
	    matrix-swap-rows!
	    matrix-row-multiply!
	    matrix-column-add!
	    matrix-row-echelon!
	    matrix-reduced-row-echelon!
	    matrix-identity))

(define (matrix? obj)
  "Predicate testing whether OBJ is an array of rank 2."
  (and (array? obj)
       (= 2 (array-rank obj))))

(define (matrix-column arr col)
  "Return the column at index COL in 2-dimensional array ARR as a vector."
  (define row-count (car (array-dimensions arr)))
  (list->vector
   (array->list
    (make-shared-array arr (lambda (i) (list i col)) row-count))))

(define (matrix-row arr row)
  "Return the row at index ROW in a 2-dimensional array ARR as a vector."
  (list->vector
   (array->list
    (array-cell-ref arr row))))

(define (vector-argmax vec less)
  "Return the index of the largest element of vector VEC, as determined
by the comparison procedure LESS."
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

(define (matrix-swap-rows! arr idx-a idx-b)
  "Mutate ARR by performing an in-place swap of the rows at index IDX-A
and IDX-B."
  (define col-count (cadr (array-dimensions arr)))
  (define row-a (make-array 0 col-count))
  (array-copy! (array-cell-ref arr idx-a) row-a)
  (let ((row-b (array-cell-ref arr idx-b)))
    (array-cell-set! arr row-b idx-a)
    (array-cell-set! arr row-a idx-b)))

(define (matrix-row-multiply! arr row-idx factor)
  "Mutate ARR by multiplying each element of the row at ROW-IDX by
FACTOR."
  (define row (array-cell-ref arr row-idx))
  (array-map! row (lambda (element) (* element factor)) row))

(define (matrix-column-add! arr col-idx sumand)
  "Mutate ARR by adding SUMAND to each element of the column at COL-IDX."
  (define col (array-cell-ref (transpose-array arr 1 0) col-idx))
  (array-map! col (cute + sumand <>) col))

(define (abs-< a b)
  (< (abs a) (abs b)))

(define (matrix-row-echelon! arr)
  "Mutate ARR into (unreduced) row-echelon form.  Algorithm taken from
pseudocode found at
https://en.wikipedia.org/wiki/Gaussian_elimination#Pseudocode"
  (define dims (array-dimensions arr))
  (define row-count (car dims))
  (define col-count (cadr dims))
  (let loop ((pivot-row 0)
	     (pivot-col 0))
    (when (and (< pivot-row row-count) (< pivot-col col-count))
      (let* ((column-k (matrix-column arr pivot-col))
	     (pivot-i (vector-argmax column-k abs-<)))
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
  "Mutate ARR into reduced row-echelon form."
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
  "Return a new square matrix initialized to the N by N identity matrix."
  (define arr (make-array 0 n n))
  (array-index-map! arr (lambda (i j) (if (= i j) 1 0)))
  arr)
