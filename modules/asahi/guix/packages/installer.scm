(define-module (asahi guix packages installer)
  #:use-module ((gnu packages linux) #:prefix linux:)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix build installer)
  #:use-module (asahi guix build modules)
  #:use-module (asahi guix build utils)
  #:use-module (asahi guix images base)
  #:use-module (asahi guix images edge)
  #:use-module (asahi guix images gnome)
  #:use-module (asahi guix images plasma)
  #:use-module (asahi guix images sway)
  #:use-module (gnu image)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu system image)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export ($ASAHI_INSTALLER_OS_PATH))

(define %asahi-installer-os-path
  "share/asahi-installer/os")

(define $ASAHI_INSTALLER_OS_PATH
  (search-path-specification
   (variable "ASAHI_INSTALLER_OS_PATH")
   (files (list %asahi-installer-os-path))))

(define-public asahi-installer-icon
  (package
    (name "asahi-installer-icon")
    (version "0.0.1")
    (source (local-file "../files/bootlogo.svg"))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((source (assoc-ref inputs "source"))
                     (target (string-append #$output "/share/asahi-installer"))
                     (icon (format #f "~a/asahi-guix.icns" target))
                     (dimensions (list 128)))
                (define (filename width)
                  (format #f "ax~a.png" width width))
                (mkdir-p target)
                (for-each (lambda (width)
                            (invoke "convert"
                                    "-background" "none"
                                    "-resize" (format #f "~ax~a!" width width)
                                    source
                                    (filename width)))
                          dimensions)
                (apply invoke (append (list "png2icns" icon)
                                      (map filename dimensions)))))))))
    (home-page "https://guix.gnu.org/")
    (native-inputs (list imagemagick inkscape libicns))
    (synopsis "Asahi Guix installer icon")
    (description "This package provides the Asahi Guix installer icon.")
    (license license:expat)))

(define (make-installer-package image)
  (let ((flavor (capitalize (symbol->string (image-name image)))))
    (package
      (name (format #f "~a-installer-package" (image-name image)))
      (version "0.0.1")
      (source #f)
      (build-system copy-build-system)
      (arguments
       (list
        #:modules '((asahi guix build installer)
                    (guix build copy-build-system)
                    (guix build utils))
        #:phases
        (with-extensions (list guile-json-4)
          (with-imported-modules (source-module-closure
                                  '((asahi guix build installer))
                                  #:select? import-asahi-module?)
            #~(modify-phases %standard-phases
                (delete 'unpack)
                (replace 'install
                  (lambda* (#:key inputs #:allow-other-keys)
                    (make-asahi-installer-package
                     (list (assoc-ref inputs "asahi-guix-installer-image"))
                     #:data-file #$(format #f "~a-~a.json"
                                           (image-name image)
                                           (package-version (current-guix)))
                     #:icon (string-append
                             (assoc-ref inputs "asahi-guix-installer-icon")
                             "/share/asahi-installer/asahi-guix.icns")
                     #:output-dir (string-append #$output "/" #$%asahi-installer-os-path)
                     #:package-version #$(package-version (current-guix))))))))))
      (home-page "https://github.com/asahi-guix/channel")
      (native-inputs `(("asahi-guix-installer-icon" ,asahi-installer-icon)
                       ("asahi-guix-installer-image" ,(system-image image))
                       ("util-linux" ,util-linux)
                       ("p7zip" ,p7zip)))
      (native-search-paths
       (list $ASAHI_INSTALLER_OS_PATH))
      (synopsis (format #f "~a installer package" flavor))
      (description (format #f "This package provides the ~a package
for the Asahi Linux installer." flavor))
      (license license:expat))))

(define-public asahi-guix-base-installer-package
  (make-installer-package asahi-guix-base-image))

(define-public asahi-guix-edge-installer-package
  (make-installer-package asahi-guix-edge-image))

(define-public asahi-guix-gnome-installer-package
  (make-installer-package asahi-guix-gnome-image))

(define-public asahi-guix-plasma-installer-package
  (make-installer-package asahi-guix-plasma-image))

(define-public asahi-guix-sway-installer-package
  (make-installer-package asahi-guix-sway-image))
