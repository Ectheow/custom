(import (srfi srfi-64)
	(srfi srfi-8)
	(custom system)
	(rnrs)
	(ice-9 rdelim))
;;(start/stop-autoreap #t)


(test-begin "run*")

(test-equal
    "hello"
  (receive (maps pid)
      (let ((pipe (run* '((write () (1)))
			(lambda ()
			  (exec 'echo 'hello))))) pipe)
    (read-line (car maps))))

(test-equal
    ;; it was sexier to do "helloerror" until I realized this was non-deterministic
    ;; because you don't know if stdout or stderr will come through first.
    "hellohello"
  (receive (maps pid)
      (run* '((write () (1 2)))
	    (lambda ()
	      (exec 'perl '-e
		    "print \"hello\\n\"; warn \"hello\\n\";")))
    (string-append
     (read-line (car maps))
     (read-line (car maps)))))

(test-equal
    "hello"
  (receive (maps pid)
      (run* '((write () (1 2))
	      (read () (0)))
		  (lambda () (exec 'cat '-)))
    (let ((out (car maps))
	  (in (cadr maps)))
      (begin
	(display "hello\n"
		 in)
	(flush-all-ports)
	(close in)
	(read-line out)))))

(test-error
 &error
 (receive (maps pid)
     (run* '((write (1 2) ())) (lambda () (exec 'echo 'hello)))
   (read-line (car maps))))
(flush-all-ports)

(test-end "run*")
(test-begin "pipe*")

(test-equal
    "hello"
    (receive (maps pid)
	(run*
	 '((write () (1)))
	 (lambda ()
	   (pipe*
	    (lambda () (exec 'echo 'hello))
	    (lambda () (exec 'cat '-)))))
      (read-line (car maps))))

(flush-all-ports)
(test-equal
    "hello world"
  (receive (maps pid)
      (run*
       '((write () (1)))
       (lambda ()
	 (pipe*
	  (lambda ()
	    (exec 'echo 'hello 'world))
	  (lambda ()
	    (exec 'cat '-))
	  (lambda ()
	    (exec 'cat '-))
	  (lambda ()
	    (exec 'cat '-)))))
    (read-line (car maps))))
(flush-all-ports)

(test-equal
    "hello world"
  (receive (maps pid)
      (run* '((write () (1)) (read () (0)))
	    (lambda ()
	      (pipe*
	       (lambda ()
		 (exec 'cat '-))
	       (lambda ()
		 (exec 'cat '-)))))
    (let ((intopipe
	   (cadr maps))
	  (outofpipe
	   (car maps)))
      (begin
	(display "hello world\n" intopipe)
	(close intopipe)
	(read-line outofpipe)))))

(test-end "pipe*")

(test-begin "run-strings*")

(test-equal
    "hello\n"
  (receive (out err)
      (run/strings*
       (lambda () (exec 'echo 'hello)))
    out))

(test-equal "hello\nworld\n"
  (receive (out err)
      (run/strings*
       (lambda ()
	 (exec 'perl '-e "print \"hello\\nworld\\n\"")))
    out))
(test-end "run-strings*")

(test-begin "run-strings+*")

(test-equal
    (sort (list "hello\n"
		"world\n")
	  string<?)
  (receive (out err)
      (run/strings+*
       '((write () (2)) (write () (1)))
       (lambda ()
	 (exec 'perl '-e
	       "print \"hello\\n\"; warn \"world\\n\";")))
    (sort
     (list out err)
     string<?)))

(test-end "run-strings+*")
     
    

	    

   

