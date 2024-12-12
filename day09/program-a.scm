#!/usr/bin/env guile
!#

(use-modules (ice-9 textual-ports)
	     (srfi srfi-1)
	     (srfi srfi-43))

(define (read-disk-map port)
  (map (compose string->number string) (string->list (get-line port))))

(define (make-filesystem disk-map)
  (let loop ((map-rest disk-map)
	     (filesystem '())
	     (file-id 0)
	     (block? #t))
    (if (null? map-rest)
        (list->vector filesystem)
	(loop (cdr map-rest)
	      (append
	       filesystem
	       (map (const (and block? file-id)) (iota (car map-rest))))
	      (if block? (1+ file-id) file-id)
	      (not block?)))))

(define (compact-filesystem! filesystem)
  (let loop ((forward 0)
	     (backward (1- (vector-length filesystem))))
    (when (< forward backward)
      (cond ((not (vector-ref filesystem backward))
	     (loop forward (1- backward)))
	    ((not (vector-ref filesystem forward))
	     (vector-swap! filesystem forward backward)
	     (loop (1+ forward) (1- backward)))
	    (else (loop (1+ forward) backward))))))

(define (compute-checksum filesystem)
  (fold (lambda (file-id block-id total)
	  (+ total (* (or file-id 0) block-id)))
	0 (vector->list filesystem) (iota (vector-length filesystem))))

(define (main)
  (let ((filesystem (make-filesystem (read-disk-map (current-input-port)))))
    (compact-filesystem! filesystem)
    (display (compute-checksum filesystem))
    (newline)))

(main)
