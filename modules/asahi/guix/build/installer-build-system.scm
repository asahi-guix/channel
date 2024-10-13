(define-module (asahi guix build installer-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (ice-9 ftw)
  #:use-module (asahi guix build sfdisk)
  #:use-module (asahi guix build installer package)
  #:use-module (guix build utils)
  #:export (%standard-phases
            installer-build))

;; Phases

(define* (configure #:key out-of-source? #:allow-other-keys)
  (let* ((abs-srcdir (getcwd))
         (srcdir (if out-of-source?
                     (string-append "../" (basename abs-srcdir))
                     ".")))
    (format #t "source directory: ~s (relative from build: ~s)~%"
            abs-srcdir srcdir)
    (if out-of-source?
        (begin
          (mkdir "../build")
          (chdir "../build")))
    (format #t "build directory: ~s~%" (getcwd))))

(define* (unpack #:key source #:allow-other-keys)
  (let* ((table (sfdisk-list source))
         ;; (partitions (build-partitions package table))
         )
    (format #t "Table: ~a~%" table)
    (format #t "DIR: ~a~%" (getcwd))
    table))

;; (unpack
;;  #:source "/gnu/store/ga2p4vcyb6znclqda446hrkg99kpir3f-disk-image")

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
  #t)

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'check)
    (replace 'configure configure)
    (replace 'unpack unpack)
    (replace 'build build)
    (replace 'install install)))

(define* (installer-build #:key inputs (phases %standard-phases)
                          #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
