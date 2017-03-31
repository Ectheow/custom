(define-module (custom tests common))
(define (get-tmpfile-name&port)
  (let* ([name (string-copy "/tmp/guile-test-XXXXXX")]
	 [port (mkstemp! name)])
    (cons name  port)))
(export get-tmpfile-name&port)
