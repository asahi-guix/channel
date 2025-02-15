(define-module (asahi guix packages firmware)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix packages installer)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages python-build)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public asahi-fwextract
  (package
    (name "asahi-fwextract")
    (version "0.7.8")
    (source
     (origin
       (inherit %asahi-installer-source)
       (modules '((guix build utils)))
       (snippet
        '(begin
           (with-output-to-file "entry_points.txt"
             (lambda ()
               (format #t "[console_scripts]\n")
               (format #t "asahi-fwextract = asahi_firmware.update:main")))))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'create-entrypoints 'wrap-program
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (wrap-program (string-append out "/bin/asahi-fwextract")
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,(string-append (assoc-ref inputs "lzfse") "/lib"))))))))))
    (inputs (list lzfse))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/AsahiLinux/asahi-installer")
    (synopsis "Asahi Linux firmware extractor")
    (description "The Asahi Linux firmware extractor transform the firmware archive
provided by the Asahi Linux installer into a manifest and CPIO and TAR
archives that are compatible with the Linux kernel.")
    (license license:expat)))
