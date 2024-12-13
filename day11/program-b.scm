#!/usr/bin/env guile
!#

(use-modules (ice-9 textual-ports)
	     (srfi srfi-1)
	     (srfi srfi-26))

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


(define stones-cache (make-hash-table))
(define (count-stones generations stone)
  (define cached-result (hash-ref stones-cache `(,generations ,stone) #f))
  (or cached-result
      (let ((result (_count-stones generations stone)))
	(hash-set! stones-cache `(,generations ,stone) result)
	result)))

(define (_count-stones generations stone)
  (define stones (new-stone stone))
  (cond ((= generations 0) 1)
	((not (list? stones))
	 (count-stones (1- generations) stones))
	((= (length stones) 2)
	 (+ (count-stones (1- generations) (car stones))
	    (count-stones (1- generations) (cadr stones))))
	(else (error "wrong number of stones"))))

(define (main)
  (display
   (apply + (map (cute count-stones 75 <>)
		 (read-stones (current-input-port)))))
  (newline))

(main)
