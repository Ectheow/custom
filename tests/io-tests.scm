(use-modules (srfi srfi-64)
	     (custom io)
	     (custom processes))

(test-begin "basic-scanline")
(test-equal "hello!"
  (read-line-before-or-unread
   (open-input-string "hello!\n") 1))

(test-equal "hello!"
  (read-line-before-or-unread
   (subprocess-stdout
    (fork-and-exec '(sh -c "echo hello!"))) 1))
(test-equal #false
  (read-line-before-or-unread
   (subprocess-stdout
    (fork-and-exec '(sh -c "sleep 2; echo hello!"))) 1))


(test-equal "hello!"
  (scan-input-for-line
   (lambda (x)
     (equal? x "hello!"))
   (open-input-string
    "one\ntwo\nthree\nhello!\nworld")))

(test-equal #false
  (scan-input-for-line
   (lambda (x)
     (equal? x "hello!"))
   (open-input-string
    "one\ntwo\nthree\nfour\nfive\n")))

(test-equal #false
  (scan-input-for-line
   (lambda (x) (equal? x "hello!"))
   (subprocess-stdout (fork-and-exec '(sh -c "sleep 5; echo hello!")))
   #:timeout-secs 1))

(test-equal "hello!"
  (scan-input-for-line
   (lambda (x)
     (equal? x "hello!"))
   (subprocess-stdout (fork-and-exec '(sh -c "sleep 3; echo hello!")))
   #:timeout-secs 4))

(test-end "basic-scanline")
     
   
