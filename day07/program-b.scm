#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
	     (ice-9 receive)
	     (srfi srfi-1))

(define (read-calibration-line line)
  (let* ((split-colon (string-split line #\:))
	 (test-value (string->number (car split-colon)))
	 (numbers (map string->number
		       (string-tokenize (cadr split-colon)))))
    (values test-value numbers)))

(define (concat num-a num-b)
  (string->number
   (string-append
    (number->string num-a) (number->string num-b))))

(define operators (list + * concat))

(define (reduce-calibration numbers ops ops-spec)
  (define radix (length ops))
  (define ops-spec-mod (expt radix (1- (length numbers))))
  (define ops-indices
    (map (compose string->number string)
	 (string->list
	  (string-pad
	   (number->string
	    (modulo ops-spec ops-spec-mod)
	    radix)
	   (1- (length numbers)) #\0))))
  (fold (lambda (num op-idx prev) ((list-ref ops op-idx) prev num))
	(car numbers) (cdr numbers) ops-indices))

(define (equation-valid? test-value numbers ops)
  (define radix (length ops))
  (define ops-spec-mod (expt radix (1- (length numbers))))
  (let loop ((ops-spec 0))
    (if (= ops-spec ops-spec-mod)
	#f
	(let ((cal (reduce-calibration numbers ops ops-spec)))
	  (if (= cal test-value)
	      #t
	      (loop (1+ ops-spec)))))))

(define (sum-valid-values port)
  (let loop ((line (read-line port))
	     (calibration-result 0))
    (if (eof-object? line)
	calibration-result
	(receive (test-value numbers)
	    (read-calibration-line line)
	  (if (equation-valid? test-value numbers operators)
	      (loop (read-line port) (+ calibration-result test-value))
	      (loop (read-line port) calibration-result))))))

(define (main)
  (display (sum-valid-values (current-input-port)))
  (newline))

(main)
