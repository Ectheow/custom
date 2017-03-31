(define-module (custom io))
(use-modules (ice-9 rdelim))

(

 define (read-line-before-or-unread port timeout)
  (define (do-read-line-before/unread port timeout start curbuf)
    (cond
     ((char-ready? port)
      (let ((char (read-char port)))
	(cond
	 ((eof-object? char) char)
	 ((eq? char #\newline) curbuf)
	 ((>= (- (current-time) start) timeout)
	  (begin
	    (unread-string curbuf port)
	    #false))
	 (else
	  (do-read-line-before/unread
	   port
	   timeout
	   start
	   (string-append curbuf (string char)))))))
     (else
      (do-read-line-before/unread
       port
       timeout
       start
       curbuf))))
  (do-read-line-before/unread port timeout (current-time) ""))
(export read-line-before-or-unread)

(define* (scan-input-for-line line-pred input-port #:key [timeout-secs -1])
  (define (do-read-line input-port timeout)
    (cond
     ((>= 0 timeout) (read-line input-port))
     (else
      (read-line-before-or-unread input-port timeout))))
  (define (do-scan-input-port line-pred input-port timeout start-time)
    (let ((read-line-result (do-read-line input-port timeout)))
      (cond
       ((eq? #false read-line-result) #false)
       ((line-pred read-line-result) read-line-result)
       ((eof-object? read-line-result) #false)
       ((and (> timeout 0)
	     (>= (- (current-time) start-time) timeout)) #false)
       (else
	(do-scan-input-port
	 line-pred
	 input-port
	 timeout
	 start-time)))))
  (do-scan-input-port line-pred
		      input-port
		      timeout-secs
		      (current-time)))
(export scan-input-for-line)

(define (scan-input-for-line/error line-pred input-port)
  "scan-input-conditional-for-line/error : function, input-port -> #t or 'error
scan an input port for liens with line-pred, which returns either #t,
#f or 'error. If it returns  'error, we return (cons 'error line). If it returns
#t at any point, we return (cons #t line), otherwise, we return #f."
  (let* ((line (read-line input-port))
	 (pred-result (line-pred line)))
    (cond
     ((eq? pred-result 'error) (cons 'error line))
     ((eq? pred-result #true) (cons #true line))
     ((eof-object? input-port) #f)
     (else
      (scan-input-for-line/error line-pred input-port)))))
(export scan-input-for-line/error)

     
