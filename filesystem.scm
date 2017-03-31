(define-module (custom filesystem)
  #:export (find-files find-inner prepend-path find-ftw find-childftw))
(use-modules (ice-9 ftw)
	     (ice-9 format)
	     (ice-9 regex))

#| example
(cons "cats" (cons #(1 2 3) (cons (cons "child" (cons #(1 2 3) empty)) empty)))
ftw is:
1. (cons s (cons v ftwchildren))

ftwchildren is:
1. '()
2. (cons ftw ftwchildren)
where in all 3 s is a string, v is a vector, and ftw is a ftw.
           v-----ftw            v----ftwchild  v--------ftw
(cons (cons s (cons v '()))  (cons (cons s (cons v '())) '()))
  ^--ftwchild example
note that ftwchildren must necessarily be either empty or a list of
lists itself, e.g. a single ftw that isn't empty:
(cons (cons "wonk" (cons #(1 2 3) '())) '())

For this simple example, we can see three ftwchildren.
(cons (cons "wonk" (cons #(1 2 3) '())) '())
|------------------------------------------|
  this entire thing is an ftw child.

(cons "wonk" (cons #(1 2 3) '()))
|-------------------------------|
This is the ftw in (2.). 

'()
|-|
This is an ftwchild, matching (1.).
|#

(define (find-ftw ftw rootdir matchfun whole-name)
  (let ((newroot (string-append rootdir file-name-separator-string (car ftw))))
    (cond
     ((or
       (and (not whole-name) (matchfun (car ftw) (cadr ftw)))
       (and whole-name (matchfun newroot (cadr ftw))))
      (cons
       newroot
       (find-childftw (cddr ftw) newroot matchfun whole-name)))
     (else
      (find-childftw (cddr ftw) newroot matchfun whole-name)))))

(define (find-childftw ftwchildren rootdir matchfun whole-name)
  (cond
   ((eq? '() ftwchildren) '())
   (else
    (append
     (find-ftw (car ftwchildren) rootdir matchfun whole-name)
     (find-childftw (cdr ftwchildren) rootdir matchfun whole-name)))))

;; a FILE-TREE-WALK (short ftw) is :
;; 1. empty or
;; 2. (cons n v ftw1 ftw2), where n is a string, v is a vector, and ftw[1,2] are ftws. or
;; 3. (cons n v ftw) where n is a string and v is a vector and ftw is a file-tree-walk.

;; find : string, string-or-procedure [, boolean] -> list-of-strings
;; find all files starting at rootdir whose name matches pat.
;; if whole-name is set to true, match against the entire path from rootdir.
;; Returns false if the directory doesn't exist.
(define* (find-files rootdir pat-or-fun #:optional (whole-name #false))
  "find-files : string string-or-function [,boolean] -> list-of-strings
Find a file in the filesystem, starting at rootdir, and including
it in the list to return if:
  1. pat-or-fun is a string and string-match returns non-false
  2. pat-or-fun is a function and, given two arguments, the file name
     and the stat vector for the current file, it returns non-false.

If whole-name is true, the entire path of the file will be passed to
the pattern matching function.

An exception is thrown if rootdir isn't a directory or doesn't exist,
or if pat-or-fun is not a function or string."
  (if (not (and (file-exists? rootdir) (eq? (stat:type (stat rootdir)) 'directory)))
      (throw 'bad-file (format #f "the file ~a doesn't exist or isn't a dir" rootdir))
      (let ((walk (file-system-tree rootdir)))
	(cond
	 ((string? pat-or-fun)
	  (let ((matchfun (lambda (x y) (string-match pat-or-fun x))))
	    (find-ftw walk rootdir matchfun whole-name)))
	 ((procedure? pat-or-fun)
	  (find-ftw walk rootdir pat-or-fun whole-name))
	 (else
	  (throw 'bad-matcher))))))
