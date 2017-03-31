(use-modules (srfi srfi-64)
	     (custom filesystem)
	     (ice-9 regex))
(define make-matchfun
  (lambda (x)
    (lambda (y z)
      (string-match x y))))
  
(test-begin "prepend-path")
(test-equal
    '()
  (prepend-path "cats" '()))
(test-equal
    '("cats/one" "cats/two")
  (prepend-path "cats" '("one" "two")))
(test-end "prepend-path")

(test-begin "filesystem-find")
(test-equal
    '()
  (find-ftw '("one" #()) "." (make-matchfun "two") #false))
(test-equal
    '()
  (find-ftw '("one" #()) "."  (make-matchfun "doodle") #false))
(test-equal
    '("./hello" "./hello/cats" "./hello/cats/dogs" "./hello/cats/mice")
  (find-ftw '("hello" #() ("cats" #() ("dogs" #()) ("mice" #()))) "." (make-matchfun ".*") #false))
(test-equal
    '("./one/two.txt")
  (find-ftw
   '("one" #() ("two.txt" #()) ("spec2" #()) ("hello" #()) ("three.tyt" #()))
   "."
   (make-matchfun ".*txt$") #false))

(define wholepath-pat (make-matchfun "\\./.*/.*/.*txt$"))
(define wholepath-ftw
   '("one" #() ("two" #() ("three.txt" #()))))
(test-equal
    '("./one/two/three.txt")
  (find-ftw
   wholepath-ftw
   "."
   (lambda (x y) (begin (format #t "Processing: ~a~%" x) (wholepath-pat x y)))
   #true))

(test-equal
    '()
  (find-ftw
   wholepath-ftw
   "."
   wholepath-pat
   #false))
(test-end "filesystem-find")
