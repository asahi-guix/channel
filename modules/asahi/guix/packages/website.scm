(define-module (asahi guix packages website)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix build modules)
  #:use-module (asahi guix packages installer)
  #:use-module (gnu packages guile)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages))

(define-public asahi-guix-website
  (package
    (name "asahi-guix-website")
    (version "0.0.1")
    (source #f)
    (build-system copy-build-system)
    (arguments
     (list
      #:modules '((asahi guix build installer)
                  (guix build copy-build-system)
                  (guix build utils)
                  (srfi srfi-1))
      #:phases
      (with-extensions (list guile-json-4)
        (with-imported-modules (source-module-closure
                                '((asahi guix build installer))
                                #:select? import-asahi-module?)
          #~(modify-phases %standard-phases
              (delete 'unpack)
              (replace 'install
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((target (string-append #$output "/share/asahi-guix-website/os")))
                    (mkdir-p target)
                    (define os-dirs
                      (search-path-as-list
                       (list "share/asahi-installer/os")
                       (map cdr inputs)))
                    (define (symlink-os dir)
                      (for-each (lambda (file)
                                  (symlink file (string-append target "/" (basename file))))
                                (find-files dir)))
                    (define (merge-data sources)
                      (let ((data (map read-installer-data sources)))
                        (write-installer-data
                         (reduce merge-installer-data #f data)
                         (string-append target "/installer_data.json"))))
                    (define (find-data-files dir)
                      (find-files dir ".json"))
                    (for-each symlink-os os-dirs)
                    (merge-data (append-map find-data-files os-dirs))))))))))
    (home-page "https://www.asahi-guix.org")
    (inputs (list asahi-guix-base-installer-package
                  asahi-guix-edge-installer-package
                  ;; asahi-guix-gnome-installer-package
                  ;; asahi-guix-plasma-installer-package
                  ;; asahi-guix-sway-installer-package
                  ))
    (synopsis "Asahi Guix website")
    (description "This package provides the Asahi Guix website.")
    (license license:gpl3+)))
