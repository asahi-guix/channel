(define-module (asahi guix packages installer)
  #:use-module ((gnu packages linux) #:prefix linux:)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix build installer package)
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

(define (make-installer-package artifact-name short-name long-name image)
  (package
    (name artifact-name)
    (version "0.0.1")
    (source #f)
    (build-system copy-build-system)
    (arguments
     (list
      #:modules '((asahi guix build installer package)
                  (guix build copy-build-system)
                  (guix build utils))
      #:phases
      (with-extensions (list guile-json-4)
        (with-imported-modules (source-module-closure
                                '((asahi guix build installer package))
                                #:select? import-asahi-module?)
          #~(modify-phases %standard-phases
              (delete 'unpack)
              (replace 'install
                (lambda* (#:key inputs #:allow-other-keys)
                  (build-installer-package
                   (installer-package
                    (artifact-name #$artifact-name)
                    (short-name #$short-name)
                    (long-name #$long-name)
                    (data-file #$(format #f "~a.json" (image-name image)))
                    (disk-image (assoc-ref inputs "asahi-guix-installer-image"))
                    (icon (string-append
                           (assoc-ref inputs "asahi-guix-installer-icon")
                           "/share/asahi-installer/asahi-guix.icns"))
                    (output-dir (string-append #$output "/" #$%asahi-installer-os-path))
                    (version #$version)
                    (script (search-input-file inputs "/bin/asahi-guix-installer.sh")))))))))))
    (home-page "https://github.com/asahi-guix/channel")
    (native-inputs `(("asahi-guix-installer-icon" ,asahi-installer-icon)
                     ("asahi-guix-installer-image" ,(system-image image))
                     ("asahi-guix-installer-script" ,asahi-installer-script)
                     ("util-linux" ,util-linux)
                     ("p7zip" ,p7zip)))
    (native-search-paths
     (list $ASAHI_INSTALLER_OS_PATH))
    (synopsis (format #f "~a installer package" short-name))
    (description (format #f "Asahi Linux installer package for the ~a." long-name))
    (license license:expat)))

(define-public asahi-base-installer-package
  (make-installer-package
   "asahi-base-installer-package"
   "Asahi Guix Base"
   "Asahi GNU/Guix operating with the bare minimum"
   asahi-guix-base-image))

(define-public asahi-edge-installer-package
  (make-installer-package
   "asahi-edge-installer-package"
   "Asahi Guix Edge"
   "Asahi GNU/Guix operating system with accelerated graphics"
   asahi-guix-edge-image))

(define-public asahi-gnome-installer-package
  (make-installer-package
   "asahi-gnome-installer-package"
   "Asahi Guix Gnome"
   "Asahi GNU/Guix operating system with the Gnome desktop environment"
   asahi-guix-gnome-image))

(define-public asahi-plasma-installer-package
  (make-installer-package
   "asahi-plasma-installer-package"
   "Asahi Guix Plasma"
   "Asahi GNU/Guix operating system with the KDE Plasma desktop environment"
   asahi-guix-plasma-image))

(define-public asahi-sway-installer-package
  (make-installer-package
   "asahi-sway-installer-package"
   "Asahi Guix Sway"
   "Asahi GNU/Guix operating system with the Sway window manager"
   asahi-guix-sway-image))
