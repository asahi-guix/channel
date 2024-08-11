(define-module (asahi guix cuirass jobs)
  #:use-module (asahi guix packages ci)
  #:use-module (asahi guix systems install)
  #:use-module (gnu ci)
  #:use-module (gnu packages package-management)
  #:use-module (gnu system image)
  #:use-module (guix channels)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:export (cuirass-jobs))

(define (cuirass-jobs store arguments)

  (define systems
    (arguments->systems arguments))

  (define channels
    (let ((channels (assq-ref arguments 'channels)))
      (map sexp->channel channels)))

  (define guix
    (find guix-channel? channels))

  (define commit
    (channel-commit guix))

  (define source
    (channel-url guix))

  (parameterize ((current-guix-package
                  (channel-source->package source #:commit commit))
                 (%graft? #f))
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
