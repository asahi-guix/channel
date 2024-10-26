(define-module (asahi guix build installer-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (asahi guix installer package)
  #:use-module (guix build utils)
  #:export (%standard-phases
            installer-build))

(define* (build #:key build-dir name icon os-name os-description source #:allow-other-keys)
  (build-package (installer-package
                  (artifact name)
                  (build-dir build-dir)
                  (disk-image source)
                  (icon icon)
                  (os-description os-description)
                  (os-name os-name))))

(define* (install #:key build-dir name icon os-name os-description outputs source #:allow-other-keys)
  (install-package (installer-package
                    (artifact name)
                    (build-dir build-dir)
                    (disk-image source)
                    (icon icon)
                    (os-description os-description)
                    (os-name os-name))
                   (assoc-ref outputs "out")))

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
