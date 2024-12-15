#!/usr/bin/env guile
!#

(add-to-load-path "..")

(use-modules (ice-9 arrays)
	     (ice-9 textual-ports)
	     (scram grid))

(define (move-robot! grid direction position)
  "Mutate the GRID according to the robot movement rules, and return the
new robot position.  Assumes POSITION is a valid current position of
the robot."
  (define dirs
    '((#\^ . n) (#\v . s) (#\< . w) (#\> . e)))
  (define reverse-dirs
    '((n . s) (s . n) (e . w) (w . e)))
  (define grid-dir (assoc-ref dirs direction))
  (define reverse-dir (assoc-ref reverse-dirs grid-dir))
  (define end-of-line
    (let loop ((pos position))
      (let ((new-pos (grid-move grid-dir pos)))
	(if (or (char=? #\. (apply array-ref grid new-pos))
		(char=? #\# (apply array-ref grid new-pos)))
	    new-pos
	    (loop new-pos)))))
  (if (char=? #\. (apply array-ref grid end-of-line))
      (let loop ((pos end-of-line))
	(let* ((prev-pos (grid-move reverse-dir pos))
	       (prev-char (apply array-ref grid prev-pos)))
	  (apply array-set! grid prev-char pos)
	  (apply array-set! grid #\. prev-pos)
	  (if (char=? #\@ prev-char)
	      pos
	      (loop prev-pos))))
      position))

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
     (when (char=? #\O (array-ref grid row col))
       (set! gps-total (+ gps-total (* 100 row) col)))))
  gps-total)

(define (main)
  (define grid (read-character-grid))
  (display (sum-box-coordinates (simulate-robot! grid)))
  (newline))

(main)
