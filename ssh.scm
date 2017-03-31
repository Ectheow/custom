(define-module (custom ssh))
(use-modules (custom processes))
(define DEFAULT-PROMPT-LINE ".*[\\$#\\>]\\s*")
(define PASSWORD-PROMPT-LINE "password:\\s*")
(define PERMISSION-DENIED-LINE "Permission denied")

(define (build-prompt prompt-list)
  (cond
   ((eq? '() prompt-list) '())
   (else
    (cond
     ((not (eq? #false (caar prompt-list)))
      (append
       (cdar prompt-list)
       (build-prompt (cdr prompt-list))))
     (else
      (build-prompt (cdr prompt-list)))))))

(define* (ssh-to hostname #:key
		 [password #false]
		 [keyfile #false]
		 [username #false]
		 [config #false]
		 [prompt-regex DEFAULT-PROMPT-LINE])
  (let* ((subproc
	  (fork-and-exec (build-prompt '((#true . ("ssh" hostname))
					 (username . ("-u" username))
					 (keyfile . ("-i" keyfile))
					 (config . ("-F" config)))))))
    (cond
     ((not (eq? #false password))
      (ssh-with-password subproc prompt-regex password-prompt password))
     (else
      (ssh-with-cmdline subproc prompt-regex)))))
(export ssh-to)

(define (ssh-with-password subproc prompt-regex password-prompt password)
  (define (try-to-ssh ntimes)
    (cond
     ((eq? ntimes 0) #false)
     (else
       (let ((scanresult (scan-input-port-for-line/error
			  (lambda (x)
			    (cond
			     ((string-match password-prompt x) #true)
			     ((string-match error-password x) 'error)
			     (else #false))))))
	 (cond
	  ((pair? scanresult)
	   (
	(subprocess-stdout subproc))
       
  (begin
    (scan-input-port-for-line prompt-regex
			      (subprocess-stdout subproc))
    (write-line password (subprocess-stdin subproc))
    (ssh-get-prompt prompt-regex timeout)))
	
