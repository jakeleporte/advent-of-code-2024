#!/usr/bin/env guile
!#

(eval-when (expand load eval)
 (define parent-directory
   (string-join
    (reverse
     (cdr
      (reverse
       (string-split (dirname (current-filename))
		     (string->char-set file-name-separator-string)))))
    file-name-separator-string)))

(add-to-load-path parent-directory)

(use-modules (ice-9 arrays)
	     (ice-9 match)
	     (ice-9 textual-ports)
	     (srfi srfi-1)
	     (srfi srfi-26)
	     (scram grid))

(define (widen-grid grid)
  (list->array
   2
   (map (lambda (row)
	  (append-map (match-lambda
			(#\# '(#\# #\#))
			(#\O '(#\[ #\]))
			(#\. '(#\. #\.))
			(#\@ '(#\@ #\.)))
		      row))
	(array->list grid))))

(define (rows-to-move grid direction position)
  (let loop ((rows (list (list position))))
    (let* ((fringe (car rows))
	   (new-posits (map (cute grid-move direction <>) fringe))
	   (new-chars (map (cute apply array-ref grid <>) new-posits))
	   (new-posits-exp
	    (append-map
	     (lambda (pos)
	       (let ((curr-char (apply array-ref grid pos))
		     (left-pos (grid-move 'w pos))
		     (right-pos (grid-move 'e pos)))
		 (cond ((or (eq? direction 'e) (eq? direction 'w))
			(list pos))
		       ((and (char=? #\] curr-char)
			     (not (member left-pos new-posits)))
			(list left-pos pos))
		       ((and (char=? #\[ curr-char)
			     (not (member right-pos new-posits)))
			(list right-pos pos))
		       (else (list pos)))))
	     new-posits))
	   (new-chars-exp (map (cute apply array-ref grid <>) new-posits-exp)))
      (cond ((any (cute char=? #\# <>) new-chars-exp)
	     '())
	    ((every (cute char=? #\. <>) new-chars-exp)
	     rows)
	    (else
	     (loop (cons (filter (lambda (pos)
				   (not (char=? #\. (apply array-ref grid pos))))
				 new-posits-exp)
			 rows)))))))

(define (move-robot! grid direction position)
  "Mutate the GRID according to the robot movement rules, and return the
new robot position.  Assumes POSITION is a valid current position of
the robot."
  (define dirs
    '((#\^ . n) (#\v . s) (#\< . w) (#\> . e)))
  (define reverse-dirs
    '((n . s) (s . n) (e . w) (w . e)))
  (define grid-dir (assoc-ref dirs direction))
  (define move-rows (rows-to-move grid grid-dir position))
  (for-each
   (lambda (row)
     (let loop ((rest row))
       (when (not (null? rest))
	 (let* ((pos (car rest))
		(curr-char (apply array-ref grid pos))
		(next-pos (grid-move grid-dir pos)))
	   (apply array-set! grid #\. pos)
	   (apply array-set! grid curr-char next-pos)
	   (loop (cdr rest))))))
   move-rows)
  (if (null? move-rows)
      position
      (grid-move grid-dir position)))

(define* (simulate-robot! grid #:optional (port (current-input-port)))
  (define robot-initial-pos
    (grid-indices (lambda (c) (char=? c #\@)) grid))
  (let loop ((char (get-char port))
	     (robot-pos robot-initial-pos))
    (cond ((eof-object? char)
	   grid)
	  ((char-set-contains? char-set:whitespace char)
	   (loop (get-char port) robot-pos))
	  ((char-set-contains? (string->char-set "^v<>") char)
	   (loop (get-char port)
		 (move-robot! grid char robot-pos)))
	  (else
	   (error "invalid direction character")))))

(define (sum-box-coordinates grid)
  (define grid-copy (array-copy grid))
  (define gps-total 0)
  (array-index-map!
   grid-copy
   (lambda (row col)
     (when (char=? #\[ (array-ref grid row col))
       (set! gps-total (+ gps-total (* 100 row) col)))))
  gps-total)

(define (main)
  (define grid (widen-grid (read-character-grid)))
  (simulate-robot! grid)
  (display (sum-box-coordinates grid))
  (newline))

(main)
