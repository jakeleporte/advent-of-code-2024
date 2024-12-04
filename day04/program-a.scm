#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
	     (srfi srfi-1)
	     (srfi srfi-26))

(define (read-wordsearch port)
  (let loop ((line (read-line port))
	     (elements '()))
    (if (eof-object? line)
	(list->array 2 (reverse elements))
	(loop (read-line port)
	      (cons (string->list line) elements)))))

(define directions
  '((n . (-1 0))
    (ne . (-1 1))
    (e . (0 1))
    (se . (1 1))
    (s . (1 0))
    (sw . (1 -1))
    (w . (0 -1))
    (nw . (-1 -1))))

(define (search-word-direction ws row-idx col-idx direction word)
  (define dir (assoc-ref directions direction))
  (let loop ((rest (string->list word))
	     (row row-idx)
	     (col col-idx))
    (cond ((null? rest) #t)
	  ((not (array-in-bounds? ws row col)) #f)
	  ((not (char=? (car rest) (array-ref ws row col))) #f)
	  (else (loop (cdr rest) (+ row (car dir)) (+ col (cadr dir)))))))

(define (search-word-all-directions ws row-idx col-idx word)
  (define dirs (map car directions))
  (count
   identity
   (map (cute search-word-direction ws row-idx col-idx <> word) dirs)))

(define (count-words ws word)
  (define dims (array-dimensions ws))
  (define count-array (make-array 0 (car dims) (cadr dims)))
  (array-index-map! count-array (cute search-word-all-directions ws <> <> word))
  (reduce + 0 (fold append '() (array->list count-array))))

(define (main)
  (display (count-words (read-wordsearch (current-input-port)) "XMAS"))
  (newline))

(main)
