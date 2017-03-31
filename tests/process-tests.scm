(use-modules (srfi srfi-64)
	     (custom processes)
	     (ice-9 regex)
	     (ice-9 rdelim))


(test-begin "basic forks")

(test-equal #true
  (subprocess? (fork-and-exec (list "echo" "hello"))))

(test-equal "hello"
  (let ((subproc (fork-and-exec (list "echo" "hello"))))
    (read-line (subprocess-stdout subproc))))
(test-equal "hello"
  (let ((subproc (fork-and-exec (list "cat" "-"))))
    (begin
      (write-line "hello" (subprocess-stdin subproc))
      (close-output-port (subprocess-stdin subproc))
      (read-line (subprocess-stdout subproc)))))
(test-equal "hello"
  (let ((subproc (fork-and-exec '(cat -))))
    (begin
      (write-line "hello" (subprocess-stdin subproc))
      (close-output-port (subprocess-stdin subproc))
      (read-line (subprocess-stdout subproc)))))

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
