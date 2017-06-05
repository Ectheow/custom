(import (custom system)
	(rnrs)
	(ice-9 rdelim)
	(srfi srfi-8)
	(srfi srfi-64)
	(custom runplus-dev))

(test-begin "run+star")

(let ((fname (tmpnam)))
 (begin
   (test-equal
	"world cats"
      (receive (pid filename)
	  (run+* `((> ,fname))
		 (lambda ()
		   (exec 'echo 'world 'cats)))
	(begin
	  (wait pid)
	  (with-input-from-file
	      fname
	    (lambda () (read-line))))))
   (delete-file fname)))


(test-equal
 "hworld\n"
 (let ((port (open-output-string)))
   (receive (pid port)
       (run+* `((> ,port))
	      (lambda ()
		(exec 'echo 'hworld)))
     (begin
       (wait pid)
       (get-output-string port)))))
(let ((tmpfile (tmpnam)))
  (with-output-to-file
      tmpfile
    (lambda ()
      (display "hello, world!\n")))
  (test-equal
      "hello, world!"
      (receive (pid output-port input-port)
	  (run+* `((> () (1))
		   (< ,tmpfile))
		 (lambda ()
		   (exec 'cat '-)))
	(read-line output-port)))
  (delete-file tmpfile))

(let ((port (open-input-string "hello\n")))
  (test-equal
      "hello"
    (receive (pid output-port input-port)
	(run+* `((> () (1))
		 (< ,port))
	       (lambda ()
		 (exec 'cat '-)))
      (begin
	(wait pid)
	(read-line output-port)))))


(test-equal
    "helloworld"
    (receive (pid output-filename)
	(run+* '((> "/tmp/hello" (1 2)))
	       (lambda ()
		 (exec 'perl '-e
		       "$|=1; print \"hello\\n\";  warn \"world\\n\";")))

      (begin
	(wait pid)
	(call-with-values
	    (lambda ()
	      (with-input-from-file
		  output-filename
		(lambda ()
		  (let ((one (read-line))
			(two (read-line)))
		    (begin
		      (values one two))))))
	  string-append))))

(let ((tmpname (tmpnam)))
  (test-equal
      "helloworlddoodlecats"
    (receive (pid fname)
	(run+* `((> ,tmpname (1 2 3 4)))
	       (lambda ()
		 (let ((port1 (fdopen 1 "w"))
		       (port2 (fdopen 2 "w"))
		       (port3 (fdopen 3 "w"))
		       (port4 (fdopen 4 "w")))
		   (display "hello\n" port1)
		   (flush-all-ports)
		   (display "world\n" port2)
		   (flush-all-ports)
		   (display "doodle\n" port3)
		   (flush-all-ports)
		   (display "cats\n" port4))
		 (primitive-exit 0)))
      (wait pid)
      (call-with-values
	  (lambda ()
	    (with-input-from-file
		tmpname
	      (lambda ()
		(values
		 (read-line)
		 (read-line)
		 (read-line)
		 (read-line)))))
	string-append)))
  (delete-file tmpname))


(test-end "run+star")
