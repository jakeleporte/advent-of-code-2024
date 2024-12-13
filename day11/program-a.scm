#!/usr/bin/env guile
!#

(use-modules (ice-9 textual-ports)
	     (srfi srfi-1))

(define (read-stones port)
  (map string->number (string-tokenize (get-line port))))

(define (new-stone stone)
  (define stone-string (number->string stone))
  (define stone-string-length (string-length stone-string))
  (define half-length (/ stone-string-length 2))
  (cond ((= stone 0)
	 1)
	((even? stone-string-length)
	 (map string->number
	      (list (string-take stone-string half-length)
		    (string-drop stone-string half-length))))
	(else (* stone 2024))))

(define (new-stones stones)
  (append-map (lambda (stone) (if (list? stone)
			     stone
			     (list stone)))
	      (map new-stone stones)))

(define (evolve-stones generations initial-stones)
  (let loop ((stones initial-stones)
	     (generation 0))
    (if (= generation generations)
	stones
	(loop (new-stones stones) (1+ generation)))))

(define (main)
  (display (length (evolve-stones 25 (read-stones (current-input-port)))))
  (newline))

(main)
