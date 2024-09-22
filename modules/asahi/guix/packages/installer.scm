(define-module (asahi guix packages installer)
  #:use-module ((gnu packages linux) #:prefix linux:)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix build installer)
  #:use-module (asahi guix build modules)
  #:use-module (asahi guix images base)
  #:use-module (asahi guix images edge)
  #:use-module (asahi guix images gnome)
  #:use-module (asahi guix images plasma)
  #:use-module (asahi guix images sway)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu image)
  #:use-module (gnu system image)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (ice-9 match))

(define (make-asahi-installer name image)
  (package
    (name (format #f "asahi-installer-~a" name))
    (version "0.0.1")
    (source #f)
    (build-system copy-build-system)
    (arguments
     (list
      #:modules '((asahi guix build installer)
                  (guix build copy-build-system)
                  (guix build utils)
                  (ice-9 pretty-print))
      #:phases
      (with-extensions (list guile-json-4)
        (with-imported-modules (source-module-closure
                                '((asahi guix build installer))
                                #:select? import-asahi-module?)
          #~(modify-phases %standard-phases
              (delete 'unpack)
              (replace 'install
                (lambda* (#:key inputs #:allow-other-keys)
                  (mkdir-p #$output)
                  (make-asahi-installer-package
                   (list (assoc-ref inputs "image"))
                   #:output-dir #$output
                   #:package-version #$(package-version guix)))))))))
    (home-page "https://github.com/asahi-guix/channel")
    (native-inputs `(("image" ,(system-image image))
                     ("util-linux" ,util-linux)
                     ("p7zip" ,p7zip)))
    (synopsis (format #f "Asahi Installer ~a package" name))
    (description (format #f "Asahi Installer ~a package." name))
    (license license:expat)))

(define-public asahi-installer-base
  (make-asahi-installer "base" asahi-base-image))

(define-public asahi-installer-edge
  (make-asahi-installer "edge" asahi-edge-image))

(define-public asahi-installer-gnome
  (make-asahi-installer "gnome" asahi-gnome-image))

(define-public asahi-installer-plasma
  (make-asahi-installer "plasma" asahi-plasma-image))

(define-public asahi-installer-sway
  (make-asahi-installer "sway" asahi-sway-image))
