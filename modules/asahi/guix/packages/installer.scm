(define-module (asahi guix packages installer)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix build-system installer)
  #:use-module (asahi guix config)
  #:use-module (asahi guix systems base)
  #:use-module (asahi guix systems gnome)
  #:use-module (asahi guix systems plasma)
  #:use-module (asahi guix systems sway)
  #:use-module (asahi guix search-paths)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu system image)
  #:use-module (guix build utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:export (%asahi-installer-source))

(define (make-installer-source version hash)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/AsahiLinux/asahi-installer")
          (commit (string-append "v" version))))
    (file-name (git-file-name "asahi-installer-source" version))
    (sha256 (base32 hash))))

(define %asahi-installer-source
  (make-installer-source "0.7.8" "0yj4gn1p6cvk7d507y5l608axp72rkrn0f5f7hywhv8il9c0fs2j"))

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

(define-public asahi-installer-script
  (package
    (name "asahi-installer-script")
    (version "0.0.1")
    (source %asahi-installer-source)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((asahi guix installer script)
                  (guix build utils)
                  (guix records))
      #:builder
      #~(begin
          (use-modules (asahi guix installer script))
          (let ((source (string-append #$(package-source this-package) "/scripts/bootstrap-prod.sh"))
                (target (string-append #$output "/bin/asahi-guix-install")))
            (write-installer-script
             (installer-script
              (inherit (read-installer-script source))
              (installer-data "https://www.asahi-guix.org/installer_data.json")
              (installer-data-alt "https://www.asahi-guix.org/installer_data.json")
              (repo-base "https://www.asahi-guix.org")
              (report "https://stats.asahi-guix.org/report"))
             target)))))
    (home-page "https://github.com/AsahiLinux/asahi-installer")
    (synopsis "Asahi Guix installer script")
    (description "This package provides the Asahi Guix installer script.")
    (license license:expat)))

;; (define-public asahi-installer-os-base
;;   (package
;;     (name "asahi-installer-os-base")
;;     (version %asahi-version)
;;     (source asahi-base-os)
;;     (build-system installer-build-system)
;;     (home-page "https://codeberg.org/asahi-guix/channel")
;;     (native-search-paths (list $ASAHI_INSTALLER_OS_PATH))
;;     (arguments (list #:os-name "Asahi Guix Base"
;;                      #:os-description "Asahi Guix with the bare minimum"))
;;     (synopsis "Asahi Guix Base installer package")
;;     (description "This package provides the Asahi installer package for the Asahi Guix
;; operating system with the bare minimum.")
;;     (license license:expat)))

;; (define-public asahi-installer-os-gnome
;;   (package
;;     (inherit asahi-installer-os-base)
;;     (name "asahi-installer-os-gnome")
;;     (source asahi-gnome-os)
;;     (arguments (list #:os-name "Asahi Guix Gnome"
;;                      #:os-description "Asahi Guix with the Gnome desktop environment"))
;;     (synopsis "Asahi Guix Gnome installer package")
;;     (description "This package provides the Asahi installer package for the Asahi Guix
;; operating system with accelerated graphics and the Gnome desktop
;; environment.")))

;; (define-public asahi-installer-os-plasma
;;   (package
;;     (inherit asahi-installer-os-base)
;;     (name "asahi-installer-os-plasma")
;;     (source asahi-plasma-os)
;;     (arguments (list #:os-name "Asahi Guix Plasma"
;;                      #:os-description "Asahi Guix with the Plasma desktop environment"))
;;     (synopsis "Asahi Guix Plasma installer package")
;;     (description "This package provides the Asahi installer package for the Asahi Guix
;; operating system with accelerated graphics and the KDE Plasma desktop
;; environment.")))

;; (define-public asahi-installer-os-sway
;;   (package
;;     (inherit asahi-installer-os-base)
;;     (name "asahi-installer-os-sway")
;;     (source asahi-sway-os)
;;     (arguments (list #:os-name "Asahi Guix Sway"
;;                      #:os-description "Asahi Guix with the Sway window manager"))
;;     (synopsis "Asahi Guix Sway installer package")
;;     (description "This package provides the Asahi installer package for the Asahi Guix
;; operating system with accelerated graphics and the Sway window
;; manager.")))
