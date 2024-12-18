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

(use-modules (ice-9 textual-ports)
	     (srfi srfi-1)
	     (srfi srfi-2)
	     (srfi srfi-26)
	     (scram grid)
	     (scram io))

(define (read-byte-position port)
  (and-let* ((x-coord (read-number port))
	     (_c (expect-string port ","))
	     (y-coord (read-number port)))
    (read-char port)
    (list y-coord x-coord)))

(define (read-corrupted-grid port rows cols bytes)
  (define grid (make-array #\. rows cols))
  (let loop ((pos (read-byte-position port))
	     (total-bytes 0))
    (when (and pos (< total-bytes bytes))
      (apply array-set! grid #\# pos)
      (loop (read-byte-position port) (1+ total-bytes))))
  grid)

(define (grid-neighbors grid visited pos)
  (filter (lambda (p)
	    (and (apply array-in-bounds? grid p)
		 (char=? #\. (apply array-ref grid p))
		 (not (apply array-ref visited p))))
	  (map (cute grid-move <> pos) '(n s e w))))

(define-syntax-rule (pop! lst)
  (begin (let ((first (car lst)))
	   (set! lst (cdr lst))
	   first)))

(define-syntax-rule (push! lst elt)
  (set! lst (cons elt lst)))

(define-syntax-rule (push-back! lst elt)
  (set! lst (append lst (list elt))))

(define (bfs-ram-path grid)
  "Translated to Scheme from
https://www.geeksforgeeks.org/shortest-path-unweighted-graph/"
  (define dims (array-dimensions grid))
  (define bottom-right (map 1- dims))
  (define visited (apply make-array #f dims))
  (define fringe '((0 0)))
  (define distances (make-hash-table))
  (hash-set! distances '(0 0) 0)
  (while (not (null? fringe))
    (let* ((node (pop! fringe))
	   (nd (hash-ref distances node)))
      (apply array-set! visited #t node)
      (for-each
       (lambda (n)
	 (unless (hash-ref distances n)
	   (hash-set! distances n (1+ nd))
	   (push-back! fringe n)))
       (grid-neighbors grid visited node))))
  (hash-ref distances bottom-right))

(define (main)
  (define grid (read-corrupted-grid (current-input-port) 71 71 1024))
  (display (bfs-ram-path grid))
  (newline))

(main)
