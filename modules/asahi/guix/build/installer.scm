(define-module (asahi guix build installer)
  #:use-module (asahi guix build fdisk)
  #:use-module (guix build json)
  #:use-module (guix build utils)
  #:use-module (guix records)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (make-asahi-installer-package
            make-asahi-installer-package-main))

(define my-source
  "/gnu/store/06564cby4w29g1m5d273fa7zy6i62g9v-disk-image")

(define my-line
  "/gnu/store/06564cby4w29g1m5d273fa7zy6i62g9v-disk-image2 *    83968 4262343 4178376 15.9G 83 Linux")

(define-record-type* <asahi-installer-package-configuration>
  asahi-installer-package-configuration
  make-asahi-installer-package-configuration
  asahi-installer-package-configuration?
  (source-image asahi-installer-package-configuration-source-image))

(define option-spec
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))))

(define (string-blank? s)
  (string-match "^\\s*$" s))

(define (command-output cmd . args)
  "Execute CMD with ARGS and return its output without trailing newspace."
  (let* ((port (apply open-pipe* OPEN_READ cmd args))
         (output (read-string port)))
    (close-port port)
    (string-trim-right output #\newline)))

(define (make-asahi-installer-package disk-image)
  (format #t "Fdisk: ~a\n" disk-image)
  (pretty-print (fdisk-list disk-image))
  (newline))

(define (show-usage)
  (display "Usage: make-asahi-installer-package [options] DISK-IMAGE")
  (newline))

(define* (make-asahi-installer-package-main args)
  (let* ((options (getopt-long args option-spec))
         (args (option-ref options '() #f)))
    (if (null? args)
        (show-usage)
        (make-asahi-installer-package (car args)))))
