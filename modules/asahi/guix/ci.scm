(define-module (asahi guix ci)
  #:use-module (asahi guix images installer)
  #:use-module (asahi guix manifests)
  #:use-module (asahi guix packages)
  #:use-module (asahi guix systems base)
  #:use-module (asahi guix systems edge)
  #:use-module (asahi guix systems gnome)
  #:use-module (asahi guix systems plasma)
  #:use-module (asahi guix systems sway)
  #:use-module (gnu ci)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages)
  #:use-module (gnu system image)
  #:use-module (gnu system)
  #:use-module (guix channels)
  #:use-module (guix diagnostics)
  #:use-module (guix discovery)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (cuirass-jobs))

(define (asahi-images)
  (list asahi-installer-os-image))

(define (asahi-manifests)
  (list %asahi-packages-manifest
        %asahi-systems-manifest))

(define (arguments->channels arguments)
  (let ((channels (assq-ref arguments 'channels)))
    (map sexp->channel channels)))

(define (image-jobs store images systems)
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

(define (manifests->jobs store manifests systems)
  "Return the list of jobs for the entries in MANIFESTS, a list of file
names, for each one of SYSTEMS."

  (define (manifest-entry-job-name entry)
    (string-append (manifest-entry-name entry) "-"
                   (manifest-entry-version entry)))

  (define (manifest-entry->job entry system)
    (let* ((obj (manifest-entry-item entry))
           (drv (parameterize ((%graft? #f))
                  (run-with-store store
                    (lower-object obj system)
                    #:system system)))
           (max-silent-time (or (and (package? obj)
                                     (assoc-ref (package-properties obj)
                                                'max-silent-time))
                                3600))
           (timeout (or (and (package? obj)
                             (assoc-ref (package-properties obj) 'timeout))
                        (* 5 3600))))
      (derivation->job (manifest-entry-job-name entry) drv
                       #:max-silent-time max-silent-time
                       #:timeout timeout)))

  (let ((entries (delete-duplicates
                  (append-map manifest-entries
                              manifests)
                  manifest-entry=?)))
    (append-map (lambda (system)
                  (map (cut manifest-entry->job <> system) entries))
                systems)))

(define (package-jobs store packages systems)
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

  (define package
    (channel-source->package source #:commit commit))

  (parameterize ((current-guix-package package))
    (append (image-jobs store (asahi-images) systems)
            ;; (package-jobs store (asahi-packages) systems)
            (manifests->jobs store (asahi-manifests) systems))))
