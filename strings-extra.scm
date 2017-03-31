(define-module (custom strings-extra)
  #:export (string-join))

;; a list-of-strings is:
;; 1. empty or
;; 2. (cons s los) where s is a string and los is a list-of-strings.
;; join-los : a-los -> string
;; join a list of strings with joinstr. Example:
;; (equal? (join-los (list "one" "two" "three") " ") "one two three")
;; #true
;; (equal? (join-los '() " ") "")
;; #true
;; ""
;; the template:
;; 
(define (string-join a-los joinstr)
  (cond
   [(eq? a-los '())  ""]
   [else
    (let ([joined (join-los (cdr a-los) joinstr)])
      (cond
       [(equal? joined "") (car a-los)]
       [else (string-append (car a-los) joinstr joined)]))]))
