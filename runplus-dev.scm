(define-module (custom runplus-dev))
(import (rnrs)
	(custom system)
	(ice-9 rdelim))
  
;; complex mapping forms:
;; (> () (1 2))            ; normal mappings, where the first element is the parent fds and the second element is child fds.
;;                         ; this will return a value that is an input port you can read the output from.
;; (> (0) (1 2))           ; map the parent's standard input into the child's standard output/error. returns 
;; (< (1) (0)) 
;; 
;; (> "/tmp/file" (1 2))     ; write the child's standard out and error to /tmp/file.
;; (> #<a-port-value> (1 2)) ; Given the open port, read the child output from 1 and 2 into that port. 
;; (< #<a-port-value> (0))   ; given the opened port, write that port to the fd 0 in the child. THis can be any port.
;; (< "/tmp/file" (3 4 5))   ; open /tmp/file as a read file on fds 3 4 and 5. 
;; (< "/tmp/file" (0))       ; same as below with explicit fd spec.
;; (< "/tmp/file")      ; read stdin from the file "/tmp/file". Return that string as a value.
;; (<<<  "hello world") ; read stdin from the string "hello world". Return that string as a value.
;; (>s)              ; same as below
;; (>string)         ; write stdout to a string and return it as a value.
;; (> "/tmp/file")   ; write stdout to /tmp/file. Return the filename string as a value
;; (&> "/tmp/file")  ; write stdout and stderr to /tmp/file. Return the filename string as a value.
;;
;; a complex-mapping is:
;; 1. (cons top-specifier mapping-rest) where mapping-rest is a mapping-rest.
;;
;; a top-specifier is:
;; 1. >
;; 2. <
;; 3. >string or >s
;; 4. &>
;; 5. <<<
;;
;; a mapping-rest is:
;; 1. empty
;; 2. (cons parent-source child-specifiers) where parent-source is a parent-source and
;;    child-specifiers is a child-specifiers.
;; a child-specifiers is:
;; 1. empty
;; 2. (nonempty-listof int)
;;
;; a parent-source is:
;; 1. a string
;; 2. an opened port
;; 3. a (listof int).
;;
;; a (listof int) is:
;; 1. empty
;; 2. (cons i lon) where i is an integer and lon is a (listof int).
;;
;; a (nonempty-listof int) is
;; 1. (cons i '()) where i is an integer
;; 2. (cons i lon) where i is an integer and lon is a (nonempty-listof int).
;; 
;; run+* : list-of-complex-mappings thunk -> (values ...)
;;
;; (define (run+* alocm thunk) ...)
;; (run+* '((> () (1 2))
;;

;; examples
;; (run+* '((&> "/home/hello/one.txt")) (lambda () (exec 'echo 'hello)))
;; (values "/home/hello/one.txt")
;;
;; (run+* '((<<< "hello world") (> () (1 2)) (lambda () (exec 'cat '-))))
;; (values "hello world" #<input-port>)
;;
;; (run+* '((<<< "hello world") (>string)) (lambda () (exec 'cat '-)))
;; (values "hello world" "hello world")
;;
;;
;; a mapping-result is:
;; 1. a string
;; 2. an open port
;;
;; if any of the mappings are for strings or ports from scheme, i/o is
;; performed to completion. This means for a input port to the process
;; that the run+* procedure will only return once this port has
;; returned eof. For an output port, run+* will return once the
;; relevant input port from the child process returns eof. 


(define (%run+* who alocm pipes thunk)
  "%run+* : symbol (listof complex-mappings) thunk -> (listof mapping-result)
performs the mappings and calls thunk for the child."
  (cond
   ((null? alocm)
    (if (eq? who 'child)
	(begin
	    (thunk)
	    (exit 0))

	'()))
   
   (else
    (cons (execute-mapping who (car pipes) (car alocm))
	  (%run+* who (cdr alocm) (cdr pipes) thunk)))))

(define (%run+*-finish pipes alocm alorv)
  "%run+*-finish : (listof pipe) (listof complex-mapping) (listof return-values)
'Finish' all return values that need finishing. These are values that
were passed in as ports or input/output strings. Because:
1. for ports, you need something to 'drain' input ports and 'fill'
output ports; we don't just do a direct pipe because this won't work
for custom/string ports.
2. for strings, the same thing needs to happen except with a wrapper
around it."
  (cond
   ((null? alocm)
    '())
   (else
    (cons
     (finish-one (car pipes) (car alocm) (car alorv))
     (%run+*-finish (cdr pipes) (cdr alocm) (cdr alorv))))))

(define (finish-one pipe acm arv)
  (cond
   ((eq? '> (car acm))
    (write-finish-fun pipe (cdr acm) arv))
   ((eq? '< (car acm))
    (read-finish-fun pipe (cdr acm) arv))
   ;; ((or (eq? '<string (car acm))
   ;; 	(eq? '<s (car acm)))
   ;;  ...)
   ;; ((or (eq? '>string (car acm))
   ;; 	(eq? '>s (car acm)))
   ;;  (write-finish-string pipe (cdr acm) arv))
   ;; ((eq? '&> (car specifier))
   ;;  ...)
   (else
    (error 'finish-one
	   "Bad direction specifier"
	   acm))))

(define (write-finish-fun pipe rest-of-mapping arv)
  (cond
   ((null? rest-of-mapping)
    (error 'write-finish-fun
	   "Bad (null) > mapping"
	   rest-of-mapping))
   ((list? rest-of-mapping)
    (cond
     ((string? (car rest-of-mapping)) arv)
     ((port? (car rest-of-mapping))
      (finish-write-port 
			 pipe
			 (car rest-of-mapping)
			 (cdr rest-of-mapping)
			 arv))
     ((list? (car rest-of-mapping)) arv)
     (else (error 'write-finish-fun
		  "Bad > mapping, is not null/pair"
		  rest-of-mapping))))))

(define (read-finish-fun pipe arom arv)
  "read-finish-fun : pair mapping-rest return-value -> string or port

finish the read mapping. A read mapping needs to be 'finished' if it
was given a port, which should be drained into the write pipe."
  (cond
   ((port? (car arom))
    (finish-read-port pipe (car arom) (cdr arom) arv))
   (else arv)))

(define (finish-read-port pipe source/port destination/fds pipe-w)
  "finish-read-port : pair port (listof int) output-port -> input-port

finish a read-port mapping of the form (< #<port>) by draining the
input port given as a parameter into the output port, which is the
write-end of the pipe which was mapped accordingly earlier."
  (begin
    (drain-port->other-port source/port pipe-w)
    (close pipe-w)
    source/port))

(define (drain-port->other-port pone ptwo)
  (define (%drain-port->other-port pone ptwo c)
    (cond
     ((eof-object? c)
      (begin
	(close pone)
	(flush-all-ports)
	#t))
     (else
      (begin
	(write-char c ptwo)
	(%drain-port->other-port pone ptwo (read-char pone))))))
  
  (%drain-port->other-port pone ptwo (read-char pone)))

(define (finish-write-port  pipe source destination arv)
  (let ((pipe-to-write-to source)
	(port-to-get-from arv))
    (begin
      (drain-port->other-port port-to-get-from pipe-to-write-to)
      pipe-to-write-to)))

(define (finish-write-string  pipe source destination arv)
  (let ((strport (open-output-string)))
    (finish-write-port pipe source destination strport)
    (get-output-string strport)))


(define (run+* alocm thunk)
  "run+* : (listof complex-mapping) thunk -> (values ...)"
  (let* ((pipes (make-pipelist alocm))
	 (pid (%fork)))
    (apply
     values
     (cons pid
	   (%run+*-finish
	    pipes
	    alocm
	    (cond
	     ((eq? 0 pid)
	      (%run+* 'child alocm pipes thunk))
	     (else
	      (%run+* 'parent alocm pipes thunk))))))))
(export run+*)
  
(define (make-pipelist alocm)
  "make-pipelist : (listof complex-mapping) -> (listof pipe-or-false)
create pipes for all mappings that need them, which is basically all
non-file happings."
  (cond
   ((null? alocm) '())
   (else
    (cons (make-pipe (car alocm))
	  (make-pipelist (cdr alocm))))))

(define (make-pipe specifier)
  "make-pipe : complex-mapping -> pipe or false
"
  (cond
   ((eq? '> (car specifier))
    (make-pipe>/</&> (cdr specifier)))
   ((eq? '< (car specifier))
    (make-pipe>/</&> (cdr specifier)))
   ((or
     (eq? '<string (car specifier))
     (eq? '<s (car specifier)))
    (pipe))
   ((or
     (eq? '>s (car specifier))
     (eq? '>string (car specifier)))
    (pipe))
   ((eq? '&> (car specifier))
    (make-pipe>/</&> (cdr specifier)))
   (else
    (error 'make-pipe
	   "Bad specifier in mapping"
	   specifier))))

(define (make-pipe>/</&> mapping-rest)
  "make-pipe> : mapping-rest -> pipe or false
given a > mapping-rest, make the pipe if appropriate for the
particular mapping. A pipe is not appropriate for strings since they
are files, but it is appropriate for all other cases."
  (cond
   ((string? (car mapping-rest)) #f)
   ((port? (car mapping-rest))
    (pipe))
   ((or
     (null? (car mapping-rest))
     (pair? (car mapping-rest)))
    (pipe))
   (else
    (error 'make-pipe
	   "bad mapping for >, < or &>, is not a pair/port/string" mapping-rest))))



(define (execute-mapping who pipe specifier)
  "execute-mapping : symbol complex-mapping -> mapping-result
performs the mapping appropriate for who, and specified by the specifier.
"
  (cond
   ((eq? '> (car specifier))
    (write-fun who pipe (cdr specifier)))
   ;; ((eq? '&> (car specifier))
   ;;  (writeboth-fun who pipe (cdr specifier)))
   ;; ((or (eq? '<s (car specifier))
   ;; 	(eq? '<string (car specifier)))
   ;;  (read-string-fun who pipe (cdr specifier)))
   ((eq? '< (car specifier))
    (read-fun who pipe (cdr specifier)))
   ;; ((or (eq? '>s (car specifier))
   ;; 	(eq? '>string (car specifier)))
   ;;  (write-string-fun who pipe (cdr specifier)))
   (else
    (error 'fun-for-specifier
	   "bad specifier"
	   specifier))))

;; for a write-fun, we see several possible data definition
;; alternatives.
;; it is a mapping-rest. A mapping-rest can be empty, but this is an
;; error. It can also be
;; (cons parent-source child-specifiers),
;; which is a pair?.
(define (write-fun who pipe rest-of-mapping)
  "write-fun : mapping-rest -> mapping-result
performs a >, or write, mapping, according to the directions."
  (cond
   ((null?  rest-of-mapping) ; error - > needs a file or file descriptors.
    (error 'write-fun
	   "Bad (null) rest-of-mapping!"
	   rest-of-mapping))
   ((pair?  rest-of-mapping)
    (cond
     ((string? (car rest-of-mapping))
      (write-to-file who pipe (car rest-of-mapping) (cdr rest-of-mapping)))
     ((output-port? (car rest-of-mapping))
      (write-stdout-to-port who pipe (car rest-of-mapping) (cdr rest-of-mapping)))
     ((or
       (pair? (car rest-of-mapping))
       (null? (car rest-of-mapping)))
      (write-simple-mapping who pipe (car rest-of-mapping) (cdr rest-of-mapping)))))
     (else
      (error 'write-fun
	     "bad rest-of-mapping"
		 rest-of-mapping))))

(define (write-stdout-to-port who pipe sources destinations)
  (let ((destinations
	 (if (null? destinations) '((1)) destinations)))
  (cond
   ((eq? who 'child)
    (begin
      (close sources)
      (write-simple-mapping who pipe '()  destinations)))
   ((eq? who 'parent)
    (write-simple-mapping who pipe '()  destinations)))))

(define (write-to-file who pipe sources destinations)
  (cond
   ((eq? who 'child)
    (let* ((file-fdes (open-fdes sources (logior O_WRONLY
						 O_CREAT
						 O_TRUNC)))
	   (destinations (if (null? destinations)
			     '((1))
			     destinations)))
      (write-simple-mapping who (cons #f file-fdes) '() destinations)
      (when (not (eq? file-fdes 1)) (close file-fdes))))
   ((eq? who 'parent) sources))) ;; return filename.

(define (write-simple-mapping who pipe sources destinations)
  "write-simple-mapping -> symbol pipe (listof ints) (nonempty-listof ints) -> input-port
create simple mappings from sources to destinations. sources are
parent file descriptors. If anything is open on these file
descriptors, either in the parent or child, the file descriptors are
closed. A pipe is created, 
"
  (let* ((wpipe (cdr pipe))
	 (rpipe (car pipe))
	 (wfd   (if (port? wpipe) (port->fdes wpipe) wpipe))
	 (rfd   (if (port? rpipe) (port->fdes rpipe) rpipe)))
  (cond
   ((eq? who 'child)
    (begin
      (when (not (eq? #f rpipe)) (close rpipe))
      (for-each (lambda (fd)
		  (begin
		    (dup2 wfd fd)))
		(car destinations))
      wpipe))
   ((eq? who 'parent)
    (begin
      (when (not (eq? #f wpipe)) (close wpipe))
      (for-each (lambda (fd)
		  (dup2 rfd fd))
		sources)
      rpipe)))))

(define (read-fun who pipe rom)
  "read-fun : symbol pair mapping-rest -> mapping-result
performs a <, or read mapping/redirection according to the arguments
in rom. Briefly:
   1. null - this is invalid for a <, (although not a >s or <s), since
it (a read redirection) must have something to redirect to. 
   2. string - this means redirect to file, and this mapping will be
performed.
   3. port - read from a port. 
   4. (listof int) - this is a straight file descriptor mapping."
  (cond
   ((null? rom)
    (error 'read-fun
	   "You must read from something in a read map"
	   rom))
   ((pair? rom)
    (cond
     ((string? (car rom))
      (read-from-file who pipe (car rom) (cdr rom)))
     ((port? (car rom))
      (read-from-port who pipe (car rom) (cdr rom)))
     ((pair? (car rom))
      (read-do-fdes-mapping who pipe (car rom) (cdr rom)))
     (else
      (error 'read-fun
	     "Invalid read-mapping parameter in < mapping"
	     (car rom)
	     rom))))
   (else
    (error 'read-fun
	   "Mapping is not a list"
	   rom))))

(define (read-from-port who pipe source/port destination/fds)
  "read-from-port : symbol pair input-port (listof int) -> port

Map the source/port to the child fds destination/fds, unless
destination/fds is null, in which case it is mapped to stdin.
This work is trivial, the actual work is done in the 'finisher'
function which drains the output from the source/port into the write
end of the pipe."
  (let ((destination/fds
	 (if (null? destination/fds) '((0)) destination/fds)))
    (read-do-fdes-mapping who pipe '() destination/fds)))
    

(define (read-from-file who pipe source/fname destination/null)
  "read-from-file : symbol pair string null -> string 

In the child - i.e. when who is 'child - open source/fname as a
read-only file descriptor and map that to stdin. Returns the
filename."
  (cond
   ((eq? who 'child)
    (let ((read-fdes (open-fdes source/fname
				(logior O_RDONLY))))
      (read-do-fdes-mapping who (cons read-fdes #f) '() '((0)))))
   (else source/fname)))

(define (read-do-fdes-mapping who pipe source/writetofds
			      destination/childreaderfds)
  "read-do-fdes-mapping : symbol pair (listof int) (nonemptylistof int) -> port

Perform a file descriptor mapping, in which the write/input side of
the pipe is mapped/duplicated to all source/writetofds in the parent,
and the read side of the pipe is mapped to all
destination/childreaderfds in the child. The port for the relevant
side of the pipe is returned, the write side for the parent and the
read side in the child."
  (let* ((r-port (car pipe))
	 (w-port (cdr pipe))
	 (w-fd   (if (port? w-port) (port->fdes w-port) w-port))
	 (r-fd   (if (port? r-port) (port->fdes r-port) r-port)))
    (cond
     ((eq? who 'child)
      (when (not (eq? #f w-port)) (close w-port))
      (begin
	(for-each
	 (lambda (maptofd)
	   (dup2 r-fd maptofd))
	 (car destination/childreaderfds))
	r-port))
     ((eq? who 'parent)
      (begin
	(when (not (eq? #f r-port)) (close r-port))
	(for-each
	 (lambda (maptofd)
	   (dup2 w-fd maptofd))
	 source/writetofds)
	w-port))
     (else
      (error 'read-do-fdes-mapping
	     "Bad 'who' specifier"
	     who)))))
	 
      
