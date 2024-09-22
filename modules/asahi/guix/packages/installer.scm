(define-module (asahi guix packages installer)
  #:use-module ((gnu packages linux) #:prefix linux:)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix build installer)
  #:use-module (asahi guix build modules)
  #:use-module (asahi guix images base)
  #:use-module (asahi guix images base)
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
  #:use-module (ice-9 match))

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
                     (target (string-append #$output "/share/asahi-installer-icon"))
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
                  (let ((icon (string-append
                               (assoc-ref inputs "asahi-installer-icon")
                               "/share/asahi-installer-icon/asahi-guix.icns")))
                    (format #t "ICON: ~a\n" icon)
                    (make-asahi-installer-package
                     (list (assoc-ref inputs "asahi-image"))
                     #:icon icon
                     #:output-dir #$output
                     #:package-version #$(package-version guix))))))))))
    (home-page "https://github.com/asahi-guix/channel")
    (native-inputs `(("asahi-installer-icon" ,asahi-installer-icon)
                     ("asahi-image" ,(system-image image))
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
