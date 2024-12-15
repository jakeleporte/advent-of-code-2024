(define-module (scram io)
  #:use-module (ice-9 textual-ports)
  #:export (find-string
	    find-char
	    read-string
	    read-number))

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
