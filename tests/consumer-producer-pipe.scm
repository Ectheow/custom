(use-modules (ice-9 rdelim))

(let* ([pipes (pipe)]
       [pid (primitive-fork)])
  (cond
   [(not (eq? 0 pid)) ; parent - consumer
    (begin
      (close (cdr pipes))
      (move->fdes (car pipes) 0)
      (execlp "/bin/cat" "cat" "-"))]
   [else
    (close (car pipes))
    (move->fdes (cdr pipes) 1)
    (execlp "/bin/echo" "echo" "hellocats")]))

