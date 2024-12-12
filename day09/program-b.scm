#!/usr/bin/env guile
!#

(use-modules (ice-9 receive)
	     (ice-9 textual-ports)
	     (srfi srfi-1)
	     (srfi srfi-26))

(define (read-disk-map port)
  (map (compose string->number string) (string->list (get-line port))))

(define (_get-file-sizes disk-map)
  (cond ((null? disk-map) '())
	((= (length disk-map) 1) (list (car disk-map)))
	(else (cons (car disk-map)
		    (_get-file-sizes (cddr disk-map))))))

(define (get-file-sizes disk-map)
  (list->vector (_get-file-sizes disk-map)))

(define (_get-file-locations disk-map)
  (define file-count (ceiling-quotient (length disk-map) 2))
  (map (lambda (index) (apply + (take disk-map index)))
       (iota file-count 0 2)))

(define (get-file-locations disk-map)
  (list->vector (_get-file-locations disk-map)))

(define (move-item lst item idx)
  (define gone (delete item lst))
  (append (take gone idx) (list item) (drop gone idx)))

(define (compact-file-locations file-sizes file-locations)
  (define file-count (vector-length file-sizes))
  (define new-file-locations (vector-copy file-locations))
  (define file-order (iota file-count))
  (define (move-file file-num)
    (let loop ((files-rest file-order)
	       (file-idx 0))
      (when (>= (length files-rest) 2)
	(let* ((file-size (vector-ref file-sizes file-num))
	       (curr-file-num (car files-rest))
	       (next-file-num (cadr files-rest))
	       (curr-file-tail (+ (vector-ref new-file-locations curr-file-num)
				  (vector-ref file-sizes curr-file-num)))
	       (free-space (- (vector-ref new-file-locations next-file-num)
			      curr-file-tail)))
	  (cond ((>= curr-file-tail (vector-ref new-file-locations file-num))
		 #f)
		((>= free-space file-size)
		 (vector-set! new-file-locations file-num curr-file-tail)
		 (set! file-order (move-item file-order file-num (1+ file-idx))))
		(else (loop (cdr files-rest) (1+ file-idx))))))))
  (let loop ((file-num (1- file-count)))
    (when (> file-num 0)
      (move-file file-num)
      (loop (1- file-num))))
  (values file-order new-file-locations))

(define (compute-checksum file-order file-sizes file-locations)
  (let loop ((total 0)
	     (files-rest file-order))
    (if (null? files-rest)
	total
	(let* ((file-num (car files-rest))
	       (file-size (vector-ref file-sizes file-num))
	       (file-location (vector-ref file-locations file-num)))
	  (loop
	   (+ total (apply
		     +
		     (map (cute * (car files-rest) <>)
			  (iota file-size file-location))))
	   (cdr files-rest))))))

(define (main)
  (let* ((disk-map (read-disk-map (current-input-port)))
	 (file-sizes (get-file-sizes disk-map))
	 (file-locations (get-file-locations disk-map)))
    (receive (file-order new-file-locations)
	(compact-file-locations file-sizes file-locations)
      (display (compute-checksum file-order file-sizes new-file-locations))
      (newline))))

(main)
