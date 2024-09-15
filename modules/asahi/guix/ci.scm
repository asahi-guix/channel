(define-module (asahi guix ci)
  #:use-module (asahi guix images installer)
  #:use-module (gnu ci)
  #:use-module (gnu packages)
  #:use-module (gnu system image)
  #:use-module (guix diagnostics)
  #:use-module (guix discovery)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:export (cuirass-jobs))

(define (asahi-package? package)
  (let ((filename (location-file (package-location package))))
    (string-prefix? "asahi/guix/packages" filename)))

(define (asahi-packages)
  (fold-packages (lambda (package result)
                   (if (asahi-package? package)
                       (cons package result)
                       result))
                 '()
                 #:select? (const #t)))

(define (cuirass-jobs store arguments)
  (display "Asahi Guix Curiass Jobs\n")

  (define systems
    (arguments->systems arguments))

  (format #t "Systems ~a\n" systems)
  (format #t "Packages ~a\n" (asahi-packages))

  (append-map
   (lambda (system)
     (list
      (image->job store
                  asahi-installer-image
                  #:name "asahi-installer-image"
                  #:system system)))
   systems))
