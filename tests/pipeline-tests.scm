(use-modules (srfi srfi-64)
	     (custom processes)
	     (custom tests common)
	     (ice-9 regex)
	     (ice-9 rdelim))
(test-begin "pipeline-tests")

(test-equal #true
  (pipeline? (exec-pipe '((echo hello) (cat -)) #f #f #f)))

(test-equal "hello"
  (let ([pipeline (exec-pipe '((echo hello) (cat -)) #f #f #f)])
    (read-line (pipeline-stdout pipeline))))

(test-equal "hello"
  (let ([pipeline (exec-pipe '((echo hello)
			       (cat -)
			       (cat -)
			       (cat -)
			       (grep .*)) #f #f #f)])
    (read-line (pipeline-stdout pipeline))))

(let ([thepipe (exec-pipe '((cat -) (cat -)) #f #f #f)])
  (display "hello\n" (pipeline-stdin thepipe))
  (close (pipeline-stdin thepipe))
  (test-equal "hello"
    (read-line (pipeline-stdout thepipe)))
  (test-equal (length (pipeline-close thepipe)) 2))

(let* ([thepipe (exec-pipe '((echo hello)
			    (cat -)
			    (cat -)) #f #f #f)]
       [exits (pipeline-close thepipe)])
  (let loop ([exits exits])
    (cond
     [(null? exits) #t]
     [else
      (test-equal 0 (status:exit-val (cdar exits)))
      (loop (cdr exits))])))

(display "test from input\n")
;; test input from a file.
(let ([name&input-port (get-tmpfile-name&port)])
  (display "hello, world\n" (cdr name&input-port))
  (close (cdr name&input-port))
  (let* ([input-file
	  (open-input-file (car name&input-port))]
	 [pipeline (exec-pipe '((cat -) (cat -))
			      #f input-file #f)])
    (test-equal
	"hello, world"
      (read-line (pipeline-stdout pipeline)))
    (pipeline-close pipeline)))

(display "test closed pipe\n")
;; test output to a file, input from a file.
(let ([name&output-port (get-tmpfile-name&port)]
      [name&input-port (get-tmpfile-name&port)])
  (display "hello, world\n" (cdr name&input-port))
  (close (cdr name&input-port))
  (let* ([input-port (open-input-file (car name&input-port))]
	 [pipeline (exec-pipe '((cat -) (cat -))
			      (cdr name&output-port)
			      input-port #f)])
    (pipeline-close pipeline)
    (close (cdr name&output-port)))
  (let ([input-port (open-input-file (car name&output-port))])
    (test-equal
	"hello, world"
      (read-line input-port))))
    
(test-end "pipeline-tests")

(test-begin "mixed-pipeline-thread-tests")
(let* ([producer (lambda (stdin stdout)
		  (with-output-to-port
		      (lambda ()
		      (display "hello\n")
		      (display "world\n")
		      (display "helloworld\n")))
		  (close stdout)
		  (close stdin))]
      [filterer (lambda (stdin stdout)
		  (let loop ([line (read-line stdin)])
		    (cond
		     [(eof-object? line) #t]
		     [else
		      (when
		       (string-match "hello" line)
			(with-output-to-port stdout
			  (lambda ()
			    (display line))))
		      (loop (read-line stdin))])))]
      [pipe (exec-pipe
	     `(,producer
	       (cat -)
	       (cat -)
	       (grep .*)
	       ,filterer) #f #f #f)])
      (test-equal
	  "hello"
	(read-line (pipeline-stdout pipe)))
      (test-equal "helloworld"
	(read-line (pipeline-stdout pipe))))

(test-end "mixed-pipeline-thread-tests")

