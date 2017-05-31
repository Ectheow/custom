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

(define (run+* alocm thunk)
o  (cond
   ((null? alocm) ...)
   (else
    ... (fun-for-mapping (car alocm)) ...
    ... (run* (rest alocm) thunk) ...)))

(define (fun-for-specifier specifier)
  (cond
   ((eq? '> (car specifier))
    (write-fun (cdr specifier)))
   ((eq? '&> (car specifier))
    (writeboth-fun (cdr specifier)))
   ((eq? '<<< (car specifier))
    (read-string-fun (cdr specifier)))
   ((eq? '< (car specifier))
    (read-fun (cdr specifier)))
   ((or (eq? '>s (car specifier))
	(eq? '>string (car specifier)))
    (write-string-fun (cdr specifier)))
   (else
    (error 'fun-for-specifier
	   "bad specifier"
	   specifier))))

(define (write-fun rest-of-mapping)
  (cond
   ((string? (car rest-of-mapping))
    ... (car rest-of-mapping) ...
    ... (cdr rest-of-mapping) ...)
   ((pair? (car rest-of-mapping))
    ... (car rest-of-mapping) ...
    ... (cdr rest-of-mapping) ...)
   ((output-port? (car rest-of-mapping))
    ... (car rest-of-mapping) ...
    ... (cdr rest-of-mapping) ...)
   (else
    (error 'write-fun
	   "Bad parent data mapping"
	   rest-of-mapping))))

(define (writeboth-fun rest-of-mapping)
  (cond
   ((
    




			   
    
    
    
