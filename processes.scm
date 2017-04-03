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
  (make-pipeline stdin stdout subprocs)
  pipeline?
  (stdout pipeline-stdout set-pipeline-stdout)
  (stdin pipeline-stdin set-pipeline-stdin)
  (subprocs pipeline-subprocs set-pipeline-subprocs))
(export make-pipeline
        pipeline?
        pipeline-stdout
        pipeline-stdin
        pipeline-subprocs)

(define (pipejoint-stdin pipejoint)
  (cond
   [(pipe-procedure? pipejoint)
    (pipe-procedure-stdin pipejoint)]
   [else
    (subprocess-stdin pipejoint)]))
(export pipejoint-stdin)

(define (pipejoint-stdout pipejoint)
  (cond
   [(pipe-procedure? pipejoint)
    (pipe-procedure-stdout pipejoint)]
   [else (subprocess-stdout pipejoint)]))
(export pipejoint-stdout)

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
  "with-opened-subprocess : subprocess thunk -> (values any cons)
With the subprocess, which has been opened by fork-and-exec, execute
thunk with the current output and inputports set to the subprocess'
standard in and standard out, respectively. Returns two values: whatver the thunk
returned, and the waitpid result. autoreap is temporarily turned off,
if it's on."
  (let ((old-output-port (current-output-port))
	(old-input-port (current-input-port)))
  (dynamic-wind
    (lambda ()
      (set-current-output-port (subprocess-stdin subproc))
      (set-current-input-port (subprocess-stdout subproc)))
    (lambda ()
      (values (thunk) (wait (subprocess-pid subproc))))
    (lambda ()
      (map (lambda (port)
	     (close port))
	   (list (subprocess-stdout subproc)
		 (subprocess-stderr subproc)
		 (subprocess-stdin subproc)))
      (set-current-output-port old-output-port)
      (set-current-input-port old-input-port)))))

      
;;   (let ((old-output-port (current-output-port))
;; 	(old-input-port (current-input-port)))
;;     (set-current-output-port (subprocess-stdin subproc))
;;     (set-current-input-port (subprocess-stdout subproc))
;;     (let ((ret (thunk)))
;;       (set-current-output-port old-output-port)
;;       (set-current-input-port old-input-port)
;;       (close-input-port (subprocess-stdout subproc))
;;       (close-input-port (subprocess-stderr subproc))
;;       (close-output-port (subprocess-stdin subproc))
;;       (values
;;        ret
;;        (if (not *autoreap*)
;; 	   (waitpid (subprocess-pid subproc) 0)
;; 	   #nil)))))
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
will be executed with fork-and-exec."
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
                    (lambda () (proc (car stdinpair) (cdr stdoutpair))))]
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
    
    (exec-pipe (make-pipeline stdin #f '())
	       list-of-pipejoints
	       stdout stdin stderr))
(export exec-pipe)

(define *autoreap* #f)
(define *reaped* '())

(define (start/stop-autoreap which)
  "start-autoreap -> boolean
install a signal handler that responds to SIGCHLD and automatically
reaps all child processes."
  (let ([handler (lambda (signum)
		   (do ()
		       ((eq? (catch #t
			       (lambda ()
				 (set! *reaped*
				   (cons (waitpid WAIT_ANY WNOHANG)
					 *reaped*)))
			       (lambda (key . args) #t)) #t) #t)))])
    (case which
      ((#t)
       (set! *autoreap* #t)
       (sigaction SIGCHLD handler))
      (else
       (set! *autoreap* #f)
       (sigaction SIGCHLD SIG_DFL)))))
(export start/stop-autoreap)
 
(define (%fork)
  (flush-all-ports)
  (primitive-fork))

(define (fork/pipe thunk)
  "fork/pipe : list-of-connsspecs, thunk -> number
conns is a list-of-connspecs, which is a list of lists of numbers,
where each list starts with a number which is the file descriptor on
the parent side that should be getting input from all the other
descriptors on the child's side (these will be dupped)."
  (let* ((pipe-r/w (pipe))
	 (r (car pipe-r/w))
	 (w (cdr pipe-r/w))
	 (pid (%fork)))
    (cond
     [(not (zero? pid)) ; parent
      (close w)
      (move->fdes r STDIN-FD)]
     [else ; child
      (close r)
      (move->fdes w STDOUT-FD)
      (thunk)])))

(define (pipe* . thunks)
  (letrec ((piper (lambda (thunks)
		    (let ((thunk (car thunks))
			  (thunks (cdr thunks)))
		      (cond
		       [(pair? thunks)
			(begin
			  (fork/pipe thunk)
			  (piper thunks))]
		       [else (thunk)])))))
    (cond
     [(pair? thunks) (piper thunks)]
     [else (error "no thunks passed to pipe*")])))
		      
(define (exec program . args)
  (cond
   [(string-index program #/)
    (apply execl program program args)]
   [else (apply execlp program program args)]))

;; a subprocess has some number of file descriptors, which may be read
;; or write descriptors, which we wish to connect to descriptors on
;; the parent's side. We do this with pipes; typically on the parent
;; side we don't care what file descriptor specifically we use,
;; although we may. therefore, we have:
;; for parent-side reads:
;; 1. a possible parent file descriptor that maps to
;; 2. a definite child file descriptor.
;; questions:
;; how exactly do we want to communicate with the subprocess -
;; anonymous ports or filedescriptors? There are any number of ways
;; that you can communicate with a subprocess over file descriptors, a
;; flexible approach may be interesting but  what really is the goal?
;; How do we mean subprocesses to be used, and how are they usually
;; used? Does a good implementation enable typical use cases, or all
;; use cases?
;; there are several possible use-cases for a process:
;; 1. specific -  open a process and get its stdout into a string
;; 2. specific - open a process and gt its stdout/stderr into a string
;; 3. specific - open a process and get its stdout into a port
;; 4. specific - opena  process and get its stdout, stdin, and stderr
;; into a port.
;; 5. nonspecific - open a process, map a series of ports to
;; filedescriptors, and return the 'other ends' of the new
;; descriptors. 
;; 6. nonspecific - open a process, map a series of ports to
;; filedescriptors, and map their corresponding pipe ends to another
;; set of filedescriptors on the parent process side.
;;
;; we can implement all these from a function that does 5 or 6, and use
;; wrapper functions for 1-4.
;; It would be advantageous to have:
;; 1. call-in-new-process - forks and calls a thunk in a new child process.
;; 2. portmap - given a set of r/w pipes and additional info, map
;; these to appropriate file descriptors - to be called after forking
;; in either a child or parent process.
;; 3. %exec - basic exec, looks up a path more or less.
;;
;; file descriptor mapping
;; mapped file descriptors can work by duping a descriptor any number
;; of times. For example, we can map stdout and stderr on the child
;; side to a single port or file descriptor on the parent side by
;; duping one file descriptor to 1 and 2.
;; fork
;; on parent: for each mapping, close the opposite file descriptor to
;; the one we want (i.e. if the mapping is for reading on the child,
;; close the read), and, if a desired fd is specificed, move the port
;; to that fd - if there are multiple fds that we want to map, dup those.
;; on child: for each mapping, do the mirror - given the pipe
;; direction, close the descriptors we don't want, 
;; this suggests we want several peices of info for each pipe/connection:
;; - the 'direction' - canonically this should be for the child.
;; - the set of fds on the parent we want - if the direction is
;; 'read', that means that we should map the port to one of these fds
;; on the parent, and dup to all the rest.
;; - the set of fds on the child we want
;;
;; (cons direction (cons list-of-parent-fds (cons list-of-child-fds
;; 
;; a map-spec is:
;; (cons s (cons lon1 (cons lon2 empty)))
;; where s is a symbol, lon1 and 2 are list-of-numbers.
;; 
;; '(read (1 2 3) (0))
;; would mean that we want the child to read on 0 from the parent, and
;; that for the parent, all output to 1 2 and 3 ends up going to a
;; single write fd that is piped to 0 on the child.
;; '(read () (0))
;; '(read #nil (0))
;; '(read #f (0))
;; would mean we don't care what the parent FD# is, just map it, and
;; make sure it writes to 0 on the child.
;; note: this is a single mapping. For multiple mappings, we could:
;; '((read (1 2) (0)) (write (0) (3)))
;; for example, or
;; '((read (1 2) (0)) (write (0) #nil))
;; since we only call a thunk which has no way of getting ports, we
;; should problably call it an error to not map any of the child's
;; descriptors.
;; generic usecase:
;; (run '((read #nil (0)) (write #nil (1))) (lambda () (%exec (cat -))))
;; returns three values, one for the child's read port on the parent, and another
;; for the child's write port on the parent, and another with the
;; child's pid. The parent can then use
;; these ports to communicate with the cat process.

(define (run* maps thunk)
  (let* ((pipes (map pipe maps))
	 (%domaps (lambda (pipe maps)
		    (move->fdes pipe (car maps))
		    (for-each (lambda (fd) (dup2 pipe fd))
			      (cdr maps))))
	 (do-one-map (lambda (pipe fdspec side)
		       (cond
			[(eq? 'parent side)
			 

	 (domaps (lambda (pipes fdspecs side)
		     (let ((spec-dir (car fdspec))
			   (r (car pipe))
			   (w (cdr pipe)))
		       (cond
			[(and (eq? parent/child 'parent)
			      (eq? spec-dir 'read))
			 (close r)
			 (when (caddr fdspec) (%domaps w fdspec))]
			[(and (eq? parent/child 'child)
			      (eq? spec-dir 'read))
			 (close w)
			 (%domaps r fdspec)]
			[(and (eq? parent/child 'parent)
			      (eq? spec-dir 'write))
			 (close w)
			 (when (caddr fdspec) (%domaps r fdspec))]
			[(and (eq? parent/child 'child)
			      (eq? spec-dir 'write))
			 (close r)
			 (%domaps w fdspec)])))))
	 (pid (%fork)))
    (cond
     [(zero? pid)
      ((domaps 'child) pipes maps)]
     [else
      ((domaps 'parent) pipes maps)]))
  
(define (%open-subprocess thunk
			  stdin-r/w
			  stdout-r/w
			  stderr-r/w)
  (let ([pid (%fork)])
    (cond
     [(not (zero? pid)) ; parent
      (for-each close-when-port
		(map cdr (list stderr-r/w stdout-r/w)))
      (close-when-port (car stdin-r/w))]
     [else
      (move->fdes 
     
      
     


(define (pipeline-close pipeline)
  "pipeline-close : pipeline -> list-of-cons
if the module is in autoreap mode, return nil. Otherwise, for each
process in the list-of-procs in the pipeline, waitpid for each - this
will block until each one is finished. It is a no-op returning '()
when autoreap is on."
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
  (let*
      ((close-if-not-false (lambda (pipe)
			     (if (not (eq? #f pipe)) (close pipe))))
       (param->pipepair (lambda (param dir)
			  (if (not (eq? #f param))
			      (case dir
				((in input)
				 (cons param #f))
				((out output)
				 (cons #f param))
				(else (throw 'invalid-pipe-direction)))
			      (pipe))))
       (proc-stdout-pipes (param->pipepair stdout 'out))
       (proc-stderr-pipes (param->pipepair stderr 'out))
       (proc-stdin-pipes  (param->pipepair stdin 'in))
       (pipes-and-fds (list
		       (list 'out proc-stdout-pipes STDOUT-FD)
		       (list 'in proc-stdin-pipes STDIN-FD)
		       (list 'out proc-stderr-pipes STDERR-FD)))
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
	  (map (lambda (it)
		 (let ((dir (car it))
		       (inport (caadr it))
		       (outport (cdadr it))
		       (destination-fd (caddr it)))
		   (case dir
		     ((out output)
		      (move->fdes outport destination-fd)
		      (when inport (close inport)))
		     ((in input)
		      (move->fdes inport destination-fd)
		      (when outport (close outport)))
		     (else (throw 'invalid-pipe-direction)))))
	       pipes-and-fds)
	  ;; close input sides for stdout and stderr, since these are to
	  ;; read from, and the child will be writing to
	  ;; them. vice-versa for stdin.
	  (apply execlp (cons (car argv) argv))))
       (else
	(begin
	  (map (lambda (it)
		 (let ((dir (car it))
		       (inport (caadr it))
		       (outport (cdadr it)))
		   (case dir
		     ((out output) (when outport (close outport)))
		     ((in input) (when inport (close inport))))))
	       pipes-and-fds)
	  ;; (close-if-not-other-false (cdr proc-stdout-pipes) (car proc-stdout-pipes))
	  ;; (close-if-not-other-false (cdr proc-stderr-pipes) (car proc-stderr-pipes))
	  ;; (close-if-not-other-false (car proc-stdin-pipes) (cdr proc-stdin-pipes))
	  (make-subprocess
	   (cdr proc-stdin-pipes)
	   (car proc-stdout-pipes)
	   (car proc-stderr-pipes)
	   chld)))))))

(define (->pid pid/proc)
  (cond
   [(subprocess? pid/proc)
    (subprocess-pid pid/proc)]
   [(integer? pid/proc) pid/proc]
   [else (throw '->pid (format #f "~a is not an integer or subprocess" pid/proc))]))

(define (find-reaped-pid pid)
  (cond
   [(assq pid *reaped*) =>
    (lambda (foundpid)
      (set! *reaped*
	(filter
	 (lambda (it)
	   (not (eqv? it foundpid))) *reaped*)))]
   [else #f]))


(define (wait pid/proc)
  "wait : pid/proc -> cons or false
wait for the pid or proc specified. If autoreap is set, we try to find
this pid in the list of reaped pids. If not, we try a direct call to
waitpid. If no pid is found either way, we return false. If it is, we
return the pair waitpid returned."
  (let ([pid (->pid pid/proc)])
    (cond
     [*autoreap*
      (find-reaped-pid pid)]
     [else
      (catch #t
	(lambda ()
	  (waitpid pid))
	(lambda (key . args) #f))])))
(export wait) 

(define (wait-for-each-pid list-of-pids)
  (cond
    [(eq? list-of-pids '()) '()]
    [else
     (cons (wait (car list-of-pids))
	   (wait-for-each-pid (cdr list-of-pids)))]))
(export wait-for-each-pid)

