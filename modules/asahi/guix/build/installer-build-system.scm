(define-module (asahi guix build installer-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (asahi guix build installer package)
  #:use-module (guix build utils)
  #:export (%standard-phases
            installer-build))

;; Phases

(define* (build #:key name icon inputs output-dir script source #:allow-other-keys)
  (build-installer-package
   (installer-package
    (artifact-name name)
    (default-os-name name)
    (long-name name)
    (disk-image source)
    (icon icon)
    (output-dir output-dir)
    (script script)))
  #t)

(define* (install #:key install-plan outputs #:allow-other-keys)
  ;; (format #t "Installing ~a~%" install-plan)
  ;; (format #t "Outputs ~a~%" outputs)
  #t)

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
