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
	     (ice-9 receive)
	     (srfi srfi-1)
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

(define (count-path-nodes parent-map start-pos end-pos)
  (let loop ((nodes (hash-ref parent-map end-pos)))
    (if (and (= (length nodes) 1)
	     (equal? (car (car nodes)) start-pos))
	(length nodes)
	(loop (append-map (lambda (node) (hash-ref parent-map node))
			  nodes)))))

(define-syntax-rule (pop! lst)
  (begin (let ((first (car lst)))
	   (set! lst (cdr lst))
	   first)))

(define-syntax-rule (push! lst elt)
  (set! lst (cons elt lst)))

(define (get-all-nodes maze)
  (define maze-copy (array-copy maze))
  (define maze-nodes
    (array-index-map!
     maze-copy
     (lambda (row col)
       (let ((char (array-ref maze row col)))
	 (case char
	   ((#\. #\E) (map (lambda (dir) (cons `(,row ,col) dir))
			   '(n s e w)))
	   (else '()))))))
  (append-map identity (append-map identity (array->list maze-copy))))

(define (dijkstra maze)
  "Code adapted to Scheme from
https://en.wikipedia.org/wiki/Dijkstra's_algorithm#Pseudocode"
  (define distances (make-hash-table))
  (define previous (make-hash-table))
  (define start-pos (grid-indices (cute char=? #\S <>) maze))
  (define queue (map (lambda (e) (cons e (inf))) (get-all-nodes maze)))
  (push! queue (cons (cons start-pos 'e) 0))
  (hash-set! distances (cons start-pos 'e) 0)
  (while (not (null? queue))
    (let* ((curr (pop! queue))
	   (curr-node (car curr))
	   (curr-prio (cdr curr))
	   (curr-pos (car curr-node))
	   (curr-dir (cdr curr-node))
	   (ns (neighbors maze curr-node)))
      (for-each
       (lambda (n)
	 (let* ((n-pos (car n))
		(n-dir (cdr n))
		(alt (+ (hash-ref distances curr-node)
			(if (eq? curr-dir n-dir) 1 1000))))
	   (when (<= alt (hash-ref distances n (inf)))
	     (hash-set! previous n
			(cons curr-node (hash-ref previous n '())))
	     (hash-set! distances n alt)
	     (set! queue
		   (merge (list (cons n alt))
			  (filter (lambda (e) (not (equal? (car e) n))) queue)
			  (lambda (a b) (< (cdr a) (cdr b))))))))
       ns)))
  (values distances previous))

(define (nodes->positions previous)
  (define pos-previous (make-hash-table))
  (hash-for-each
   (lambda (k v)
     (let ((pos (car k)))
       (hash-set! pos-previous
		  pos
		  (delete
		   pos
		   (delete-duplicates
		    (append (hash-ref pos-previous pos '())
			    (map car v)))))))
   previous)
  pos-previous)

(define (list-positions previous start-pos start-dir end-pos end-dir)
  (define paths
    (let loop ((nodes (list (list (cons end-pos end-dir)))))
      (if (member (cons start-pos start-dir) (car nodes))
	  nodes
	  (loop (cons (append-map (lambda (n) (hash-ref previous n)) (car nodes))
		      nodes)))))
  (delete-duplicates (map car (concatenate paths))))

(define (main)
  (define maze (read-character-grid))
  (define start-pos (grid-indices (cute char=? #\S <>) maze))
  (define end-pos (grid-indices (cute char=? #\E <>) maze))
  (receive (distances previous)
      (dijkstra maze)
    ;; End direction shouldn't matter, since the puzzle just cares
    ;; about the nodes, and `list-positions' deletes duplicates
    ;; anyway.
    (display (length (list-positions previous start-pos 'e end-pos 'n))))
  (newline))

(main)
