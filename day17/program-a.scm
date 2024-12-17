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

(use-modules (ice-9 rdelim)
	     (srfi srfi-2)
	     (scram io))

(define (read-registers port)
  (define register-a
    (and-let* ((as (read-string port "Register A: "))
	       (reg-a (read-number port))
	       (n (read-string port "\n")))
      reg-a))
  (define register-b
    (and-let* ((bs (read-string port "Register B: "))
	       (reg-b (read-number port))
	       (n (read-string port "\n")))
      reg-b))
  (define register-c
    (and-let* ((cs (read-string port "Register C: "))
	       (reg-c (read-number port))
	       (n (read-string port "\n")))
      reg-c))
  (vector register-a register-b register-c))

(define (read-program port)
  (find-string port "Program: ")
  (list->vector
   (map string->number (string-split (read-line port) #\,))))

(define (execute-program registers program)
  (define A (vector-ref registers 0))
  (define B (vector-ref registers 1))
  (define C (vector-ref registers 2))
  (define output '())
  (let jump ((ip 0))
    (if (= ip (vector-length program))
	(string-join (reverse (map number->string output)) ",")
	(let* ((inst (vector-ref program ip))
	       (lit-op (vector-ref program (1+ ip)))
	       (combo-op (case lit-op
			   ((0 1 2 3) lit-op)
			   ((4) A) ((5) B) ((6) C))))
	  (case inst
	    ((0) (set! A (ash A (- combo-op))))
	    ((1) (set! B (logxor B lit-op)))
	    ((2) (set! B (bit-extract combo-op 0 3)))
	    ((3) (when (not (zero? A)) (jump lit-op)))
	    ((4) (set! B (logxor B C)))
	    ((5) (set! output (cons (bit-extract combo-op 0 3) output)))
	    ((6) (set! B (ash A (- combo-op))))
	    ((7) (set! C (ash A (- combo-op)))))
	  (jump (+ 2 ip))))))

(define (main)
  (define initial-registers (read-registers (current-input-port)))
  (define program (read-program (current-input-port)))
  (display (execute-program initial-registers program))
  (newline))

(main)
