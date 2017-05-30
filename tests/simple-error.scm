(import (srfi srfi-1)
	(srfi srfi-8)
	(ice-9 rdelim)
	(custom system))
(receive (maps pid)
    (let ((pipe (run* '((write () (1)))
		      (lambda ()
			(exec 'echo 'hello))))) pipe)
  (read-line (car maps)))

(receive (maps pid)
    (run* '((write () (1 2)))
	  (lambda ()
	    (exec 'perl '-e
		  "print \"hello\\n\"; warn \"hello\\n\";")))
  (string-append
   (read-line (car maps))
   (read-line (car maps))))

(receive (maps pid)
    (run* '((write (1 2) ())) (lambda () (exec 'echo 'hello)))
  (read-line (car maps)))
