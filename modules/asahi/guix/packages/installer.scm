(define-module (asahi guix packages installer)
  #:use-module ((gnu packages linux) #:prefix linux:)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix build installer package)
  #:use-module (asahi guix build modules)
  #:use-module (asahi guix build utils)
  #:use-module (asahi guix build-system installer)
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
  #:use-module (guix git-download)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export ($ASAHI_INSTALLER_OS_PATH
            %asahi-installer-source
            asahi-installer-source))

(define %asahi-installer-os-path
  "share/asahi-installer/os")

(define $ASAHI_INSTALLER_OS_PATH
  (search-path-specification
   (variable "ASAHI_INSTALLER_OS_PATH")
   (files (list %asahi-installer-os-path))))

(define (asahi-installer-source version hash)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/AsahiLinux/asahi-installer")
          (commit (string-append "v" version))))
    (file-name (git-file-name "asahi-installer-source" version))
    (sha256 (base32 hash))))

(define %asahi-installer-source
  (asahi-installer-source "0.7.8" "0yj4gn1p6cvk7d507y5l608axp72rkrn0f5f7hywhv8il9c0fs2j"))

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
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'patch-generated-file-shebangs)
          (delete 'patch-shebangs)
          (delete 'patch-source)
          (delete 'patch-source-shebangs)
          (delete 'patch-usr-bin-file)
          (add-after 'unpack 'patch-source
            (lambda* _
              (substitute* "scripts/bootstrap-prod.sh"
                (("https://github.com/AsahiLinux/asahi-installer/raw/prod/data/installer_data.json")
                 "https://www.asahi-guix.org/os/installer_data.json")
                (("https://alx.sh/installer_data.json")
                 "https://www.asahi-guix.org/os/installer_data.json")
                (("https://stats.asahilinux.org/report")
                 "https://stats.asahi-guix.org/report")
                (("REPO_BASE=https://cdn.asahilinux.org")
                 "REPO_BASE=https://www.asahi-guix.org"))))
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((target (string-append #$output "/bin/asahi-guix-installer.sh")))
                (mkdir-p (dirname target))
                (copy-file "scripts/bootstrap-prod.sh" target)
                (chmod target #o755)))))))
    (home-page "https://github.com/AsahiLinux/asahi-installer")
    (synopsis "Asahi Guix installer script")
    (description "This package provides the Asahi Guix installer script.")
    (license license:expat)))

(define-public asahi-installer-package-base
  (package
    (name "asahi-installer-package-base")
    (version "0.0.1")
    (source (system-image asahi-base-os-image))
    (build-system installer-build-system)
    (home-page "https://github.com/asahi-guix/channel")
    (native-search-paths (list $ASAHI_INSTALLER_OS_PATH))
    (synopsis "Asahi Guix Base")
    (description "Asahi Linux installer package for Asahi Guix Base.")
    (license license:expat)))

(define-public asahi-installer-package-edge
  (package
    (inherit asahi-installer-package-base)
    (name "asahi-installer-package-edge")
    (source (system-image asahi-base-os-image))
    (synopsis "Asahi Guix Edge")
    (description "Asahi Linux installer package for Asahi Guix Edge.")))

(define-public asahi-installer-package-gnome
  (package
    (inherit asahi-installer-package-base)
    (name "asahi-installer-package-gnome")
    (source (system-image asahi-base-os-image))
    (synopsis "Asahi Guix Gnome")
    (description "Asahi Linux installer package for Asahi Guix Gnome.")))

(define-public asahi-installer-package-plasma
  (package
    (inherit asahi-installer-package-base)
    (name "asahi-installer-package-plasma")
    (source (system-image asahi-base-os-image))
    (synopsis "Asahi Guix Plasma")
    (description "Asahi Linux installer package for Asahi Guix Plasma.")))

(define-public asahi-installer-package-sway
  (package
    (inherit asahi-installer-package-base)
    (name "asahi-installer-package-sway")
    (source (system-image asahi-base-os-image))
    (synopsis "Asahi Guix Sway")
    (description "Asahi Linux installer package for Asahi Guix Sway.")))
