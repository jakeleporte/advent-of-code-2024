#!/usr/bin/env guile
!#

(use-modules (ice-9 textual-ports))

(define (find-string port str)
  "Search through the input PORT until either a string matching STR is
found, or the end of file is reached.  Return #f if end of file was
reached before finding STR, otherwise read past the matching string
and return #t."
  (let loop ((curr-char (get-char port))
	     (rest str))
    (cond
     ((string-null? rest)
      ;; We've grabbed one char too many, so put the last one back
      (unget-char port curr-char)
      #t)
     ((eof-object? curr-char) #f)
     ((char=? curr-char (string-ref rest 0))
      (loop (get-char port) (string-drop rest 1)))
     (else (loop (get-char port) str)))))

(define (read-number port)
  "Read the longest available sequence of digits from PORT and return the
resulting number.  Return #f if the first character from PORT is not a
digit."
  (let loop ((curr-char (get-char port))
	     (number-string ""))
    (if
     (and (not (eof-object? curr-char))
	  (char-set-contains? char-set:digit curr-char))
     (loop (get-char port)
	   (string-append number-string (string curr-char)))
     (begin
       (unget-char port curr-char)
       (if (string-null? number-string) #f
	   (string->number number-string))))))

(define (find-mul-inst port)
  "Search through the input PORT until a valid multiply instruction of
the form `mul\\([0-9]+,[0-9]+\\)' is found, and return a list of the two
numbers to multiply.  If end of file is reached before a valid
multiply instruction, return #f."
  (while (find-string port "mul(")
    (let ((first-number (read-number port)))
      (and first-number
	   (char=? #\, (get-char port))
	   (let ((second-number (read-number port)))
	     (and second-number
		  (char=? #\) (get-char port))
		  (break (list first-number second-number))))))))

(define (sum-mul-insts port)
  "Find all multiply instructions from PORT and sum all their results."
  (let loop ((numbers (find-mul-inst port))
	     (total 0))
    (if (not numbers) total
	(loop (find-mul-inst port) (+ total (apply * numbers))))))

(define (main)
  (display (sum-mul-insts (current-input-port)))
  (newline))

(main)
