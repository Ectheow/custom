(define-module (custom processes)
  #:export (fork-shexec
	    wait-for-each-pid
	    wait-for-pids
	    fork-and-exec))
(use-modules (ice-9 popen)
             (srfi srfi-9)
	     (srfi srfi-1)
             (srfi srfi-9 gnu))


(define SHELL "/bin/sh")
(define STDIN-FD 0)
(define STDOUT-FD 1)
(define STDERR-FD 2)

;; A subprocess is a structure:
;; (make-subprocess stdin stdout stderr pid)
;; where stdin is a output-port,
;; and stdout and stderr are input-ports
(define-record-type <subprocess>
  (make-subprocess stdin stdout stderr pid)
  subprocess?
  (stdout subprocess-stdout)
  (stderr subprocess-stderr)
  (stdin subprocess-stdin)
  (pid subprocess-pid))
(export make-subprocess)
(export subprocess-stdout)
(export subprocess-stdin)
(export subprocess-stderr)
(export subprocess-pid)
(export subprocess?)
(define-immutable-record-type <pipe-procedure>
  (make-pipe-procedure stdout stdin stderr thread)
  pipe-procedure?
  (stdout pipe-procedure-stdout)
  (stderr pipe-procedure-stderr)
  (stdin pipe-procedure-stdin)
  (thread pipe-procedure-thread))

(export make-pipe-procedure
        pipe-procedure?
        pipe-procedure-stdout
        pipe-procedure-stderr
        pipe-procedure-stdin)
#|
a pipeline is a structure:
(make-pipeline stdout stdin stderr subprocs)
where stdout, stdin, and stderr are input, output, and input ports,
and subprocs is a list of <subprocess> structures.
|#
(define-immutable-record-type <pipeline>
  (make-pipeline stdout stdin subprocs)
  pipeline?
  (stdout pipeline-stdout set-pipeline-stdout)
  (stdin pipeline-stdin set-pipeline-stdin)
  (subprocs pipeline-subprocs set-pipeline-subprocs))
(export make-pipeline
        pipeline?
        pipeline-stdout
        pipeline-stdin
        pipeline-subprocs)

(define (fork-shexec command)
  " fork-shexec : list-of-strings -> number
Fork and exec command by passing it to /bin/sh -c as a single joined string"
  (let ([child (primitive-fork)])
    (cond
     [(= child 0) (apply execl (list SHELL SHELL "-c" (string-join command " ")))]
     [else child])))

(define (atom->string an-atom)
  (cond
   ((symbol? an-atom)
    (symbol->string an-atom))
   ((number? an-atom)
    (number->string an-atom))
   ((string? an-atom)
    an-atom)
   (else (throw
	  'atom-to-string
	  "atom should be symbol number or string"))))

(define (stringify-list alos)
  (cond
   ((null? alos) '())
   (else
    (cons
     (atom->string (car alos))
     (stringify-list (cdr alos))))))
		     

(define (with-opened-subprocess subproc thunk)
  (let ((old-output-port (current-output-port))
	(old-input-port (current-input-port)))
    (set-current-output-port (subprocess-stdin subproc))
    (set-current-input-port (subprocess-stdout subproc))
    (let ((ret (thunk)))
      (set-current-output-port old-output-port)
      (set-current-input-port old-input-port)
      (close-input-port (subprocess-stdout subproc))
      (close-input-port (subprocess-stderr subproc))
      (close-output-port (subprocess-stdin subproc))
      (values
       ret
       (waitpid (subprocess-pid subproc))))))
(export with-opened-subprocess)

(define* (fork-and-exec argv #:optional [stdout #f] [stdin #f] [stderr #f])
  "fork-and-exec : list-of-strings-or-symbols -> <subprocess>
Fork and execute argv as a child process. Returns a subprocess structure.
If argv is a list of symbols we first transorm it to a list of
strings. This just wraps fork-and-exec-strings."
  (fork-and-exec-strings (stringify-list argv) stdout stdin stderr))
(export for-and-exec)

#|
a list-of-pipejoints is:
1. '()
2. (cons a loa) where a is a pipejoint and loa is a
   list-of-argvs.
a pipejoint is either:
1. a pipeline-procedure
2. a list-of-argvs

a pipeline-procedure is a function:
(pipeline-procedure i o)
where i and o are input and output pipes, respectively.

a pipeline is:
(make-pipeline stdout stdin stderr subprocs)
where stdout, stdin, and stderr are input, output, and input ports,
and subprocs is a list of <subprocess> structures.

A subprocess is a structure:
(make-subprocess stdin stdout stderr pid)
where stdin is a output-port,
and stdout and stderr are input-ports

a list-of-subprocesses is:
1. '()
2. (cons s los) where s is a subprocess structure and los is a list-of-subprocesses.

To create a pipeline we need as an end result: 
a list of processes or procedures that is executing, where each
process is writing to the next process' standard in with their
standard out, until the last subprocess/procedure, whose standard out is the
standard out of the entire pipeline. The standard in of the pipeline
is the standard in of the first process.

Contract of fork-and-exec:
  fork-and-exec-strings : list-of-strings port port port -> subprocess-record
Fork and execute argv as a child process, using execlp, which scans
path for us. If any of stdout stdin or stderr are not false, we dup
that port or filedesc to it's relevant filedescriptor.
 (apply execlp (cons (car argv) argv))
and return a subprocess record.

To do this:
run execlp normally for the first process. Take the standard out for
that, and apply fork-and-exec the next time with a non-false standard
in port, and save the standard out. 
|#


(define (exec-pipe list-of-pipejoints stdout stdin stderr)
  "exec-pipe : list-of-pipejoints -> pipeline
Create a pipeline for each argv or procedure in the list, each argv
will be executed with for-and-exec."
  (define (append-subprocess-to-pipe pipeline argv stdout stdin stderr)
    (let ([subproc (fork-and-exec argv stdout stdin stderr)])
      (cons
       (set-pipeline-subprocs
        pipeline
        (cons subproc (pipeline-subprocs pipeline)))
       subproc)))

  (define (append-procedure-to-pipe pipeline proc stdout stdin stderr)
    (let* ([stdoutpair (if (eq? #f stdout) (pipe) (cons #f stdout))]
           [stdinpair (if (eq? #f stdin) (pipe) (cons stdin #f))]
           [thread (call-with-new-thread
                    (lambda () (proc (cdr stdoutpair) (car stdinpair))))]
           [pipeproc (make-pipe-procedure (car stdoutpair)
                                          (cdr stdinpair)
                                          #f thread)])
      (cons
       (set-pipeline-subprocs
        pipeline
        (cons
         pipeproc
         (pipeline-subprocs pipeline)))
       pipeproc)))

  (define (pipejoint-stdin pipejoint)
    (cond
      [(pipe-procedure? pipejoint)
       (pipe-procedure-stdin pipejoint)]
      [else
       (subprocess-stdin pipejoint)]))
  (define (pipejoint-stdout pipejoint)
    (cond
     [(pipe-procedure? pipejoint)
      (pipe-procedure-stdout pipejoint)]
     [else (subprocess-stdout pipejoint)]))
  
  (define (add-pipejoint pipejoint pipeline stdout stdin stderr)
    (cond
     [(procedure? pipejoint)
      (let ([pair (append-procedure-to-pipe
		   pipeline
		   pipejoint
		   stdout stdin stderr)])
	pair)]
     [else
      (let ([pair (append-subprocess-to-pipe
		   pipeline
		   pipejoint
		   stdout stdin stderr)])
	pair)]))
  
  (define (exec-pipe pipeline list-of-pipejoints stdout stdin stderr)
    (cond
     [(null? (cdr list-of-pipejoints))
      (let* ([pair (add-pipejoint (car list-of-pipejoints)
				  pipeline
				  stdout stdin #f)]
	     [reversed-pipe (set-pipeline-subprocs
			     (set-pipeline-stdout
			      (car pair)
			      (pipejoint-stdout (cdr pair)))
			     (reverse
			      (pipeline-subprocs
			       (car pair))))])
	(set-pipeline-stdin
	 reversed-pipe
	 (pipejoint-stdin (car (pipeline-subprocs reversed-pipe)))))]
     [else
      (let ([pair (add-pipejoint (car list-of-pipejoints)
				 pipeline
				 #f stdin #f)])
	(exec-pipe (car pair)
		   (cdr list-of-pipejoints)
		   stdout
		   (pipejoint-stdout (cdr pair))
		   stderr))]))
    
    (exec-pipe (make-pipeline #f stdin '())
	       list-of-pipejoints
	       stdout stdin stderr))
(export exec-pipe)

(define (pipeline-close pipeline)
  (wait-for-each-pid
   (map subprocess-pid (pipeline-subprocs pipeline))))
(export pipeline-close)

(define (fork-and-exec-strings argv stdout stdin stderr)
  "fork-and-exec-strings : list-of-strings port port port -> subprocess-record
Fork and execute argv as a child process, using execlp, which scans
path for us. If any of stdout stdin or stderr are not false, we dup
that port or filedesc to it's relevant filedescriptor.
 (apply execlp (cons (car argv) argv))
and return a subprocess record."
  (define (close-if-not-false pipe)
    (if (not (eq? #f pipe)) (close pipe)))
  (let*
      ((proc-stdout-pipes (if (not (eq? #f stdout)) (cons #f stdout) (pipe)))
       (proc-stderr-pipes (if (not (eq? #f stderr)) (cons #f stderr) (pipe)))
       (proc-stdin-pipes  (if (not (eq? #f stdin)) (cons stdin #f) (pipe)))
       (close-if-not-false (lambda (pipe)
			     (if (not (eq? #f pipe)) (close pipe))))
       (close-if-not-other-false (lambda (pipe other)
				   (if (not (eq? #f other)) (close pipe)))))
    (flush-all-ports)
    (let ((chld (primitive-fork)))
      (cond
       ((eq? chld 0)
	(begin
	  ;; close all other ports.
	  (port-for-each
	   (lambda (port)
	     (when (and (file-port? port)
			(not (or (equal? port (cdr proc-stdout-pipes))
				 (equal? port (cdr proc-stderr-pipes))
				 (equal? port (car proc-stdin-pipes)))))
	       (close port))))
	  ;; to dups on the remaining ports.
	  (move->fdes (cdr proc-stdout-pipes) STDOUT-FD)
	  (move->fdes (cdr proc-stderr-pipes) STDERR-FD)
	  (move->fdes (car proc-stdin-pipes) STDIN-FD)
	  ;; (dup2 (port->fdes (cdr proc-stdout-pipes)) STDOUT-FD)
	  ;; (dup2 (port->fdes (cdr proc-stderr-pipes)) STDERR-FD)
	  ;; (dup2 (port->fdes (car proc-stdin-pipes)) STDIN-FD)
	  ;; close input sides for stdout and stderr, since these are to
	  ;; read from, and the child will be writing to
	  ;; them. vice-versa for stdin.
	  (close-if-not-false (car proc-stdout-pipes)) 
	  (close-if-not-false (car proc-stderr-pipes))
	  (close-if-not-false (cdr proc-stdin-pipes))
	  (apply execlp (cons (car argv) argv))))
       (else
	(begin
	  (close-if-not-other-false (cdr proc-stdout-pipes) (car proc-stdout-pipes))
	  (close-if-not-other-false (cdr proc-stderr-pipes) (car proc-stderr-pipes))
	  (close-if-not-other-false (car proc-stdin-pipes) (cdr proc-stdin-pipes))
	  (make-subprocess
	   (cdr proc-stdin-pipes)
	   (car proc-stdout-pipes)
	   (car proc-stderr-pipes)
	   chld)))))))

(define (wait-for-each-pid list-of-pids)
  (cond
    [(eq? list-of-pids '()) '()]
    [else
     (cons (waitpid (car list-of-pids) 0)
	   (wait-for-each-pid (cdr list-of-pids)))]))
(export wait-for-each-pid)

