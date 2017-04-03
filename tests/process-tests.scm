(use-modules (srfi srfi-64)
	     (srfi srfi-1)
	     (custom processes)
	     (ice-9 regex)
	     (ice-9 rdelim))


(test-begin "basic forks")

(test-equal #true
  (subprocess? (fork-and-exec (list "echo" "hello"))))
(waitpid WAIT_ANY 0)

(test-equal "hello"
  (let ((subproc (fork-and-exec (list "echo" "hello"))))
    (read-line (subprocess-stdout subproc))))
(waitpid WAIT_ANY 0)

(test-equal "hello"
  (let ((subproc (fork-and-exec (list "cat" "-"))))
    (begin
      (write-line "hello" (subprocess-stdin subproc))
      (close-output-port (subprocess-stdin subproc))
      (read-line (subprocess-stdout subproc)))))
(waitpid WAIT_ANY 0)

(test-equal "hello"
  (let ((subproc (fork-and-exec '(cat -))))
    (begin
      (write-line "hello" (subprocess-stdin subproc))
      (close-output-port (subprocess-stdin subproc))
      (read-line (subprocess-stdout subproc)))))
(waitpid WAIT_ANY 0)

(test-equal "1 2 3"
  (with-opened-subprocess
   (fork-and-exec '(echo 1 2 3))
   (lambda () (read-line))))

(test-equal "1 2 3 4 5 world"
  (with-opened-subprocess
   (fork-and-exec '("echo" 1 2 3 "4" "5" world))
   (lambda () (read-line))))

(let*
    ([template (string-copy "/tmp/guile-testfile-XXXXXX")]
     [outputport (mkstemp! template)]
     [subproc (fork-and-exec '(echo hello) outputport)])
  (begin
    (waitpid (subprocess-pid subproc))
    (with-input-from-file template
      (lambda ()
	(test-equal "hello" (read-line))))
    (delete-file template)))
(let*
    ([infile-name (string-copy "/tmp/guile-in-test-XXXXXX")]
     [outfile-name (string-copy "/tmp/guile-out-test-XXXXXX")]
     [infile-port (mkstemp! infile-name)]
     [outfile-port (mkstemp! outfile-name)])
  (begin
    (close infile-port)
    (with-output-to-file infile-name
      (lambda ()
	(display "hello-world\n"))))
  (let* ([infile-port (open-input-file infile-name)]
	 [subproc (fork-and-exec '(cat -) outfile-port infile-port)])
    (begin
      (waitpid (subprocess-pid subproc))
      (close outfile-port)
      (with-input-from-file outfile-name
	(lambda ()
	  (test-equal "hello-world" (read-line)))))))
(test-end "basic forks")

(test-begin "reaping")
(start/stop-autoreap #f) ;; we should not reap processes.
(let ([subproc (fork-and-exec '(echo hello))])
  (test-equal
      (cons (subprocess-pid subproc) 0)
    (waitpid (subprocess-pid subproc))))
(let ([procspecs '((echo hello)
		   (cat /dev/null)
		   (ls /))])
  (let ([subprocs (fold (lambda (procspec list-of-procs)
			  (cons (fork-and-exec procspec) list-of-procs)) '() procspecs)])
    (fold
     (lambda (waited subproc prev)
       (test-equal
	   (cons (car waited) (cdr waited))
	 (cons (subprocess-pid subproc) 0)))
     '()
     (wait-for-each-pid
      (map (lambda (sp) (subprocess-pid sp)) subprocs))
     subprocs)))
(define (real-sleep nsecs)
  "real-sleep : number -> true
actually sleep, regardless of interrupts, for the specified number of
seconds, which may be  a float."
  (define (real-sleep-usecs usecs-left)
    (cond
     [(<= usecs-left 0) #t]
     [else (real-sleep-usecs (usleep usecs-left))]))
  (real-sleep-usecs (* 1000000 nsecs)))

(start/stop-autoreap #t)
(let ([subproc (fork-and-exec '(echo hello))])
  (real-sleep 3)
  (test-error #t
	      (waitpid WAIT_ANY WNOHANG))) ;; wnohang throws an error if there are no more.

(let* ([procspecs '((echo hello)
		    (echo dogs)
		    (grep doodles /dev/null)
		    (ls /)
		    (cat /dev/null))]
       [subprocs (fold (lambda (procspec list-of-procs)
			 (cons (fork-and-exec procspec) list-of-procs))
		       '()
		       procspecs)])

  (real-sleep 3)
  (for-each
   (lambda (it)
     (test-error
      #t
      (waitpid (subprocess-pid it))))
   subprocs))
(test-end "reaping")
