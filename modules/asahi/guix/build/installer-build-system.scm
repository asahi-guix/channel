(define-module (asahi guix build installer-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (asahi guix build installer package)
  #:use-module (guix build utils)
  #:export (%standard-phases
            installer-build))

;; Phases

(define* (build #:key name icon inputs output-dir script source version #:allow-other-keys)
  (build-package
   (installer-package
    (artifact-name name)
    (default-os-name name)
    (long-name name)
    (disk-image source)
    (icon icon)
    (output-dir (string-append output-dir "/share/asahi-installer/os"))
    (script script)
    (version version))))

(define* (install  #:key name icon inputs output-dir script source version #:allow-other-keys)
  (install-package
   (installer-package
    (artifact-name name)
    (default-os-name name)
    (disk-image source)
    (icon icon)
    (long-name name)
    (output-dir (string-append output-dir "/share/asahi-installer/os"))
    (script script)
    (version version))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'check)
    (delete 'configure)
    (delete 'unpack)
    (replace 'build build)
    (replace 'install install)))

(define* (installer-build #:key inputs (phases %standard-phases)
                          #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
