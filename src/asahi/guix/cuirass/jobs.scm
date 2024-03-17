(define-module (asahi guix cuirass jobs)
  #:use-module (asahi guix packages ci)
  #:use-module (asahi guix system install)
  #:use-module (gnu ci)
  #:use-module (gnu system image)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:export (cuirass-jobs))

(define (cuirass-jobs store arguments)

  (define systems
    (arguments->systems arguments))

  (parameterize ((%graft? #f))
    (append-map
     (lambda (system)
       (list
        (image->job store
                    (image-with-os
                     iso9660-image
                     asahi-installation-operating-system)
                    #:name "asahi-guix-iso9660-image"
                    #:system system)))
     systems)))
