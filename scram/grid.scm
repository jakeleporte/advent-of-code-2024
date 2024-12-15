(define-module (scram grid)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:export (read-digit-grid
	    read-character-grid
	    print-character-grid
	    grid-indices
	    grid-move))

(define* (read-character-grid #:optional (port (current-input-port)))
  "Read a rectangular grid of characters from PORT and return the
resulting rank-2 array of characters."
  (let loop ((line (read-line port))
	     (rows '()))
    (cond ((eof-object? line)
	   (if (null? rows) #f (list->array 2 (reverse rows))))
	  ((and (not (null? rows))
		(not (= (length (car rows)) (string-length line))))
	   (unget-string port line)
	   (list->array 2 (reverse rows)))
	  (else
	   (loop (read-line port) (cons (string->list line) rows))))))

(define* (grid-indices pred grid #:optional start-pos)
  "Return the list of indices `'(row-index column-index)' of the first
element in GRID which satisfies PRED.  GRID is searched in row-major
order, starting at START-POS, if specified, or at `'(0 0)'.  Return #f
if no such element is found."
  (define grid-dims (array-dimensions grid))
  (define unused-copy (apply make-array #f grid-dims))
  (define matching-indices #f)
  (array-copy! grid unused-copy)
  (call/cc
   (lambda (return)
     (array-index-map!
      unused-copy
      (lambda (row col)
	(when (pred (array-ref grid row col))
	  (set! matching-indices `(,row ,col))
	  (return #t))))))
  matching-indices)

(define* (read-digit-grid #:optional (port (current-input-port)))
  "Read a rectangular grid of digits from PORT and return the resulting
rank-2 array of numbers."
  (define grid (read-character-grid port))
  (array-map! grid (compose string->number string) grid)
  grid)

(define* (print-character-grid grid #:optional (port (current-output-port)))
  "Print a grid of characters to PORT, with a trailing newline."
  (for-each
   (lambda (row)
     (display (list->string row) port)
     (newline port))
   (array->list grid)))


(define (grid-move direction position)
  "Given a list of two indices representing a POSITION in a rank-2 array,
return a new position based on the indicated DIRECTION, a symbol
representing a direction in a grid where '(0 0) is the top-left
position."
  (map + position
       (match direction
	 ('n '(-1 0))
	 ('ne '(-1 1))
	 ('e '(0 1))
	 ('se (1 1))
	 ('s '(1 0))
	 ('sw '(1 -1))
	 ('w '(0 -1))
	 ('nw (-1 -1))
	 (_ (error "direction must be n, s, e, w, or ne, se, sw, nw")))))
