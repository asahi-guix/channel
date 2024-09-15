(define-module (asahi guix ci)
  #:use-module (asahi guix images base)
  #:use-module (asahi guix images installer)
  #:use-module (asahi guix systems base)
  #:use-module (asahi guix systems desktop)
  #:use-module (asahi guix systems install)
  #:use-module (gnu ci)
  #:use-module (gnu packages)
  #:use-module (gnu system image)
  #:use-module (guix channels)
  #:use-module (guix diagnostics)
  #:use-module (guix discovery)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:export (cuirass-jobs))

(define (asahi-images)
  (list asahi-base-image))

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

(define (arguments->channels arguments)
  (let ((channels (assq-ref arguments 'channels)))
    (map sexp->channel channels)))

(define (image-jobs store systems images)
  (append-map (lambda (system)
                (map (lambda (image)
                       (image->job store image #:system system))
                     images))
              systems))

(define* (package-job store package system #:key cross? target (suffix ""))
  (let ((job-name (string-append (package-name package) "." system suffix)))
    (parameterize ((%graft? #f))
      (let* ((drv (if cross?
                      (package-cross-derivation store package target system
                                                #:graft? #f)
                      (package-derivation store package system
                                          #:graft? #f)))
             (max-silent-time (or (assoc-ref (package-properties package)
                                             'max-silent-time)
                                  3600))
             (timeout (or (assoc-ref (package-properties package)
                                     'timeout)
                          72000)))
        (derivation->job job-name drv
                         #:max-silent-time max-silent-time
                         #:timeout timeout)))))

(define (package-jobs store systems packages)
  (append-map (lambda (system)
                (map (lambda (package)
                       (package-job store package system))
                     packages))
              systems))

(define (cuirass-jobs store arguments)

  (define systems
    (arguments->systems arguments))

  (define channels
    (arguments->channels arguments))

  (define guix
    (find guix-channel? channels))

  (define commit
    (channel-commit guix))

  (define source
    (channel-url guix))

  (append
   (image-jobs store systems (asahi-images))
   (package-jobs store systems (asahi-packages))))
