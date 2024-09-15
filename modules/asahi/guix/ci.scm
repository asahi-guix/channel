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

(define (image-jobs store systems images)
  (let ((packages (asahi-packages)))
    (append-map
     (lambda (system)
       (map (lambda (image)
              (image->job store image #:system system))
            images))
     systems)))

(define package->job
  (@@ (gnu ci) package->job))

(define (package-jobs store systems packages)
  (let ((packages (asahi-packages)))
    (append-map
     (lambda (system)
       (map (lambda (package)
              (package->job store package system))
            packages))
     systems)))

(define (cuirass-jobs store arguments)
  (define systems
    (arguments->systems arguments))

  (let ((packages (asahi-packages)))
    (package-jobs store systems packages)))
