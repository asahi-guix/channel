(define-module (asahi guix packages installer)
  #:use-module ((gnu packages linux) #:prefix linux:)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix build installer)
  #:use-module (asahi guix build modules)
  #:use-module (asahi guix images base)
  #:use-module (asahi guix images edge)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu system image)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (ice-9 match))

(define-public asahi-installer
  (package
    (name "asahi-installer")
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
    (native-inputs `(("image" ,(system-image asahi-base-image))
                     ("util-linux" ,util-linux)
                     ("p7zip" ,p7zip)))
    (synopsis "Asahi Guix boot logo pacakge")
    (description "The package providing the Asahi Guix boot logo.")
    (license license:expat)))
