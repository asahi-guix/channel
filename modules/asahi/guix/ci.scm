(define-module (asahi guix ci)
  #:use-module (gnu ci)
  #:use-module (gnu system image)
  #:use-module (asahi guix images installer)
  #:use-module (srfi srfi-1)
  #:export (cuirass-jobs))

(define (cuirass-jobs store arguments)
  (display "Asahi Guix Curiass Jobs\n")

  (define systems
    (arguments->systems arguments))

  (format #t "Systems ~a\n" systems)

  (append-map
   (lambda (system)
     (list
      (image->job store
                  asahi-installer-image
                  #:name "asahi-installer-image"
                  #:system system)))
   systems))
