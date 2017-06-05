(define-module (custom system))
(import (ice-9 popen)
	(srfi srfi-9)
	(rnrs)
	(srfi srfi-8)
	(srfi srfi-9 gnu))
(define *autoreap* #f)
(define *reaped* '())

(define STDIN-FD 0)
(define STDOUT-FD 1)
;; system : system functions and macros, to be used (hopefully) in leiu of bash.
;;
;; in a shell replacement implemented on top of scheme, we want three
;; things: to be able to easily run a process, to be able to easily
;; run a pipeline, and to be able to easily reap children so that we
;; are not leaking resources.
;;
;; RUNNING A PROCESS
;;
;; functional:
;; run* - runs a 'process' - a thunk - and performs basic
;; file descriptor mapping.
;; 
;; run/strings* - easily get the standard out/error from a process,
;; without any input.
;;
;; run/strings+* - similar to run/strings* but specify mappings that
;; are passed to run*.
;;
;; TODO:
;; run+* - give a series of complex mappings that are like the
;; mappings to run* but also specify essentially a 'conversion' -
;; you can write a string to an input port and get strings out.
;; 
;; syntax:
;; run+ - syntax for run+*
;; RUNNING A PIPELINE
;; functional: pipe* - takes a series of thunks and maps the standard
;; outs to the standard ins of the next thunks.
;; 
;; REAPING
;; start/stop-autoreap - turns on autoreaping.
;; wait - waits for a specifig pid.

;; (define (run+* maps thunk)
  ;; (letrec
  ;;     ((perform-read-map
  ;; 	(lambda (which mapping)
  ;; 	  (case (which)
  ;; 	    ((child) (perform-child-read-mapping mapping))
  ;; 	    ((parent) (perform-parent-read-mapping mapping))
  ;; 	    (else (error "bad which" which)))))
  ;;      (perform-child-read-mapping
  ;; 	(lambda (mapping)
  ;; 	  (for-each 
  ;;     (perform-maps
  ;;      (lambda (which maps)
  ;; 	 (for-each
  ;; 	  (lambda (mapping)
  ;; 	    (case (car mapping)
  ;; 	      ((< read)
  ;; 	       (perform-read-map who (cdr mapping)))
  ;; 	      ((> write)
  ;; 	       (perform-write-map who (cdr mapping)))
  ;; 	      (else
  ;; 	       (error "bad mapping" mapping)))))))
      
  ;; (let ((pid (%fork)))
  ;;   (case pid
  ;;     ((0)
  ;;      (perform-maps
  ;; 	'child
  ;; 	maps)
  ;;      (thunk)
  ;;      (exit 0))
  ;;     (else
  ;;      (let ((mappings (perform-maps
  ;; 			'parent
  ;; 			maps)))
  ;; 	 (values mappings pid)))))))))))
(define (stringify-list alos)
  (cond
   ((null? alos) '())
   (else
    (cons
     (atom->string (car alos))
     (stringify-list (cdr alos))))))

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

(define (run* maps thunk)
  "RUN* : (cons dir (cons (listof integers) (cons (listof sources/sinks)))) thunk -> (values ...)
RUN* creates a set of mappings from a set of sources or sinks in the
parent process to source/sink file descriptors in the child. the
mappings are like this:

 '(write (1 2) (0))
 '(read (0) (3))

For each mapping, the first list item is the symbol direction of the
mapping. The second is a list of file descriptors in the child that
will be mapped into the file descriptors on the parent, which is the
second list. Thus a mapping


'(write (1 2) (0))

means we would like to map our first and second file descriptors in
this current, calling process, into file descriptor 0 of the child.

RETURN VALUES

Two values are returned
1. the 'mappings' - a list of ports which correspond to the parent
side for each port. This happens regardless of if you give the parent
a desired mapping, so you can get a port out for the parent without
telling the function to map anything to any specific file descriptors.

2. the pid - the actual pid on which you may call wait. 
"
  (letrec ((pipes (map (lambda (ignore) (pipe)) maps))
	   
	   (get-proper-pipe/close-other
	    (lambda (pipepair who dir)
	      (cond
	       [(or (and (eq? who 'parent)
			 (eq? dir 'read))
		    (and (eq? who 'child)
			 (eq? dir 'write)))
		(begin (close (car pipepair))
		       (cdr pipepair))]
	       [(or (and (eq? who 'parent)
			 (eq? dir 'write))
		    (and (eq? who 'child)
			 (eq? dir 'read)))
		(begin (close (cdr pipepair))
		       (car pipepair))]
	       [else (error
		      'run*
		      "invalid direction or who given"
		      dir
		      who)])))
	   
	   (do-dups (lambda (pipe mappings)
		      "do-dups : pipe, list-of-fds -> pipe
does actual moving/dups for the given pipe and
'mappings', where mappings are just a list of numbers."
		      (if (null? mappings)
			  pipe

			  (begin
			    (move->fdes pipe (car mappings))
			    (let looprest ((fds (cdr mappings)))
			      (cond [(null? fds) pipe]
				    [else (dup pipe (car fds))
					  (looprest (cdr fds))]))))))
	   
	   (domap (lambda (themap pipepair who)
		    (let* ((dir (car themap))
			   (parentmap (cadr themap))
			   (childmap (caddr themap))
			   (pipe (get-proper-pipe/close-other
				  pipepair
				  who
				  dir)))
		      (cond
		       [(null? childmap)
			(error 'run*
			       "null child mapping"
			       themap)]
		       [(eq? who 'child) (do-dups pipe childmap)]
		       [(eq? who 'parent) (do-dups pipe parentmap)]
		       [else
			(error 'run* "bad who" who)]))))
	   
	   (domaps
	    (lambda (maps pipes who)
	      (if (null? maps)
		  #nil
		  
	      (fold-right (lambda (a-mapping a-pipe rest)
		      (cons (domap a-mapping a-pipe who) rest))
		    '()
		    maps pipes))))

	     (pid (%fork)))
      (cond
       [(zero? pid)
	(begin
	  (guard (exception (else (begin
				    (flush-all-ports)
				    (port-for-each
				     (lambda (p) (when (file-port? p)
						   (close p))))
				    (primitive-exit 1))))

	    (port-for-each
	     (lambda (p)
	       (when (and
		      (file-port? p)
		      (null? (filter (lambda (pipe)
				       (or
					(equal? (car pipe) p)
					(equal? (cdr pipe) p)))
				     pipes)))
		 (close p))))
	    (domaps maps pipes 'child))
	  (thunk))]
       [else
	(values (domaps maps pipes 'parent) pid)])))
(export run*)

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

(define (wait pid/proc)
  "wait : pid -> cons or false
wait for the pid specified. If autoreap is set, we try to find
this pid in the list of reaped pids. If not, we try a direct call to
waitpid. If no pid is found either way, we return false. If it is, we
return the pair waitpid returned."
  (let ([pid pid/proc])
    (cond
     [*autoreap*
      (find-reaped-pid pid)]
     [else
      (catch #t
	(lambda ()
	  (waitpid pid))
	(lambda (key . args) #f))])))
(export wait)

(define (find-reaped-pid pid)
  (cond
   [(assq pid *reaped*) =>
    (lambda (foundpid)
      (set! *reaped*
	(filter
	 (lambda (it)
	   (not (eqv? it foundpid))) *reaped*)))]
   [else #f]))


(define (%fork)
  (flush-all-ports)
  (primitive-fork))
(export %fork)

(define (fork/pipe thunk)
  "fork/pipe :  thunk -> number
creates a pipe and forks. 
- On the parent side:
  the read pipe is mapped to the parent's standard in, and the write
  fd is closed. We then return an undefined value to the caller.
- On the child side:
  the write pipe is mapped to the child's standard out and the read fd
  is closed. the thunk is then called.
This is useful as a way to do part of a pipe - call it on a component
thunk, and now you're set up to call it again and set up the pipe."
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
  "pipe* : list-of-thunks -> never returns
pipe* will fork for each thunk in the argument list. For each thunk,
it calls fork/pipe on that thunk and then recurses. The last thunk is
execed in this process, so you should basically never call this
directly. The parent, or the process that is called, becomes the last
component in the pipe. Its initial standard in is inherited by it's
first child but then in the end remapped, so if you have the standard
in and out of this process (that calls pipe*) you have the pipeline stdout."
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
     [else (error 'pipe* "no thunks passed to pipe*" thunks)])))
(export pipe*)
		      
(define (exec program . args)
  (let ((program-s (atom->string program))
	(args-s (stringify-list args)))
    (cond
     [(string-index program-s #\/)
      (apply execl program-s program-s args-s)]
     [else (apply execlp program-s program-s args-s)])))
(export exec)


(define (%port->string input-port)
  (let loop ((char (read-char input-port)) (str ""))
    (cond
     [(eof-object? char) str]
     [else (loop (read-char input-port)
		 (string-append str (string char)))])))

(define (run/strings+* mappings thunk)
  "run/strings+* : list-of-mappings thunk -> (values ...)
like run* and run/strings* -- read pipes are ignored and all write
pipes are read to completion."
  (receive (mapped pid)
      (run* mappings thunk)
    (let ((ports
	   (filter (lambda (x) x)
		   (map (lambda (mapping-port fdspec)
			  (cond
			   [(eq? (car fdspec) 'write) mapping-port]
			   [else #f]))
			mapped
			mappings))))
      (begin
	(wait pid)
	(apply values (map %port->string ports))))))
(export run/strings+*)

(define (run/strings* thunk)
  "run/strings* : thunk -> string, string
run thunk, with a closed standard in. Return two values, the standard
out and standard error  text respectively.
"
  (receive (mappings pid)
      (run* '((read () (0)) (write () (1)) (write () (2))) thunk)
    (let ((out (cadr mappings))
	  (err (caddr mappings))
	  (in (car mappings)))
      (begin
	(close-output-port in)
	;; (set-port-encoding! out #f)
	(let ((stdout (%port->string out))
	      (stderr (%port->string err)))
	  (begin
	    (wait pid)
	    (values stdout stderr)))))))
(export run/strings*)
