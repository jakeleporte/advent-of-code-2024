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

(use-modules (ice-9 match)
	     (srfi srfi-26)
	     (scram grid))

(define (grid-rotate-left direction)
  (match direction
    ('n 'w)
    ('w 's)
    ('s 'e)
    ('e 'n)
    (_ (error "invalid direction"))))

(define (grid-rotate-right direction)
  (match direction
    ('n 'e)
    ('e 's)
    ('s 'w)
    ('w 'n)
    (_ (error "invalid direction"))))

(define (manhattan-distance apos bpos)
  (apply + (map abs (map - apos bpos))))

(define (neighbors maze node)
  (define position (car node))
  (define direction (cdr node))
  (define right-turn (grid-rotate-right direction))
  (define left-turn (grid-rotate-left direction))
  (filter
   (lambda (node) (not (char=? #\# (apply array-ref maze (car node)))))
   (list (cons (grid-move direction position) direction)
	 (cons position right-turn)
	 (cons position left-turn))))

(define (maze-lowest-score maze)
  "Code adapted to Scheme from
https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode"
  (define start-pos (grid-indices (cute char=? #\S <>) maze))
  (define end-pos (grid-indices (cute char=? #\E <>) maze))
  (define heuristic (cute manhattan-distance <> end-pos))
  (define costs (make-hash-table))
  (define guess-costs (make-hash-table))
  (define open-set (list (cons start-pos 'e)))
  (hash-set! costs `(,start-pos . e) 0)
  (hash-set! guess-costs start-pos (heuristic start-pos))
  (while (not (null? open-set))
    (let* ((curr-node (car open-set))
	   (curr-pos (car curr-node))
	   (curr-dir (cdr curr-node)))
      (when (equal? curr-pos end-pos)
	(break (hash-ref costs curr-node)))
      (set! open-set (cdr open-set))
      (for-each
       (lambda (neighbor)
	 (let* ((n-pos (car neighbor))
		(n-dir (cdr neighbor))
		(old-cost (hash-ref costs neighbor (inf)))
		(new-cost (+ (hash-ref costs curr-node)
			     (if (eq? curr-dir n-dir) 1 1000))))
	   (when (< new-cost old-cost)
	     (hash-set! costs neighbor new-cost)
	     (hash-set! guess-costs neighbor (+ new-cost (heuristic n-pos)))
	     (when (not (member neighbor open-set))
	       (set!
		open-set
		(merge `(,neighbor) open-set
		       (lambda (a b) (< (hash-ref guess-costs a (inf))
				   (hash-ref guess-costs b (inf))))))))))
       (neighbors maze curr-node)))))

(define (main)
  (define maze (read-character-grid))
  (define start-pos (grid-indices (cute char=? #\S <>) maze))
  (display (maze-lowest-score maze))
  (newline))

(main)
