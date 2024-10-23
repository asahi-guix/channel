(define-module (asahi guix build utils)
  #:use-module (guix build utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:export (capitalize
            command-output
            escape-label
            null->false
            string-blank?
            reset-timestamps))

(define circa-1980
  (* 10 366 24 60 60))

(define* (capitalize str #:optional (separator #\space))
  (let ((words (string-split str separator)))
    (string-join
     (map
      (lambda (word)
        (string-append
         (string-upcase (substring word 0 1))
         (substring word 1)))
      words)
     " ")))

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

(define (escape-label label)
  (regexp-substitute/global #f "[ \t]+" label 'pre "\\040" 'post))

(define (null->false x)
  (if (eq? 'null x) #f x))

(define (string-blank? s)
  (string-match "^\\s*$" s))

(define (reset-timestamps directory)
  (for-each (lambda (file)
              (let ((s (lstat file)))
                (unless (eq? (stat:type s) 'symlink)
                  (utime file circa-1980 circa-1980))))
            (find-files directory #:directories? #t)))
