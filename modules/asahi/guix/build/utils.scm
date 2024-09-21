(define-module (asahi guix build utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (command-output))

(define (command-output cmd . args)
  "Execute CMD with ARGS and return its output without trailing newspace."
  (let ((port (apply open-pipe* OPEN_READ cmd args)))
    (with-exception-handler
        (lambda (ex)
          (close-port port)
          (raise-exception ex))
      (lambda ()
        (let ((output (read-string port)))
          (if (eqv? 0 (status:exit-val (close-pipe port)))
              (string-trim-right output #\newline)
              (error (format #f "Command failed: ~a ~a\n" cmd (string-join args " "))))))
      #:unwind? #t)))
