#!/usr/bin/env guile
!#

(use-modules (ice-9 textual-ports))

(define (find-char port cs)
  "Search through the input PORT until either a string in the character
set CS is found, or the end of file is reached.  Return #f if end of
file was reached before finding a matching character, otherwise return
the matching character, and push it back to the input port."
  (let loop ((curr-char (get-char port)))
    (cond
     ((eof-object? curr-char) #f)
     ((char-set-contains? cs curr-char)
      (unget-char port curr-char)
      curr-char)
     (else (loop (get-char port))))))

(define (read-string port str)
  "Attempt to read the string STR from the current position of PORT.
Return #t and advance past the matching string if it matches,
otherwise return #f, and push any portion of unmatched string already
read back to PORT."
  (let loop ((curr-char (get-char port))
	     (curr-match "")
	     (rest str))
    (cond
     ((string-null? rest)
      (unget-char port curr-char) #t)
     ((eof-object? curr-char)
      (unget-string port curr-match) #f)
     ((char=? curr-char (string-ref rest 0))
      (loop (get-char port)
	    (string-append curr-match (string curr-char))
	    (string-drop rest 1)))
     (else
      (unget-char port curr-char)
      (unget-string port curr-match) #f))))

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

(define (read-mul-inst port)
  "Attempt to read a multiply instruction from PORT of
the form `mul\\([0-9]+,[0-9]+\\)' is found, and return a list of the
two numbers to multiply.  If end of file is reached before a valid
multiply instruction, return #f."
  (let ((first-number (read-number port)))
    (and first-number
	 (char=? #\, (get-char port))
	 (let ((second-number (read-number port)))
	   (and second-number
		(char=? #\) (get-char port))
		(list first-number second-number))))))

(define (execute-mul-program port)
  (define multiply? #t)
  (define total 0)
  (while (find-char port (string->char-set "dm"))
    (when (read-string port "do()")
      (set! multiply? #t)
      (continue))
    (when (read-string port "don't()")
      (set! multiply? #f)
      (continue))
    (when (read-string port "mul(")
      (let ((numbers (read-mul-inst port)))
	(when (and multiply? numbers)
	  (set! total (+ total (apply * numbers)))))
      (continue))
    (get-char port))
  total)

(define (main)
  (display (execute-mul-program (current-input-port)))
  (newline))

(main)
