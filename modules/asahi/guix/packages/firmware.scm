(define-module (asahi guix packages firmware)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix packages installer)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public asahi-firmware
  (package
    (name "asahi-firmware")
    (version "1.0.0")
    (source (cond
             ((getenv "ASAHI_GUIX_FIRMWARE_SOURCE")
              (local-file (getenv "ASAHI_GUIX_FIRMWARE_SOURCE")))
             ((file-exists? "/boot/efi/vendorfw/firmware.cpio")
              (local-file "/boot/efi/vendorfw/firmware.cpio"))
             ((file-exists? "/run/.system-efi/vendorfw/firmware.cpio")
              (local-file "/run/.system-efi/vendorfw/firmware.cpio"))
             (else #f)))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let ((out (assoc-ref %outputs "out"))
                (cpio (search-input-file %build-inputs "/bin/cpio"))
                (source (assoc-ref %build-inputs "source")))
            (if (and source (file-exists? source))
                (let ((target (string-append out "/lib/firmware")))
                  (invoke cpio "-idv" "-F" source)
                  (mkdir-p target)
                  (copy-recursively "vendorfw" target))
                (begin
                  (display "WARNING: Apple Silicon firmware was not found !!!\n\n")
                  (display "Please set either the ASAHI_GUIX_FIRMWARE_SOURCE environment variable
to a file named firmware.cpio, or make it in one of the following
locations available:\n\n")
                  (display "- /boot/efi/vendorfw/firmware.cpio\n")
                  (display "- /run/.system-efi/vendorfw/firmware.cpio\n\n")
                  (mkdir-p out)))))))
    (native-inputs (list cpio))
    (home-page "https://github.com/asahi-guix/channel")
    (synopsis "Asahi Guix firmware for Apple Silicon")
    (description "The Asahi Guix firmware package uses the Apple Silicon firmware from
the local machine as source.  The Apple Silicon firmware is
propriatary and can not be packaged.")
    (license license:expat)))

(define-public asahi-fwextract
  (let ((hash "0yj4gn1p6cvk7d507y5l608axp72rkrn0f5f7hywhv8il9c0fs2j"))
    (package
      (name "asahi-fwextract")
      (version "0.7.8")
      (source
       (origin
         (inherit (asahi-installer-source version hash))
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
      (home-page "https://github.com/AsahiLinux/asahi-installer")
      (synopsis "Asahi Linux firmware extractor")
      (description "The Asahi Linux firmware extractor transform the firmware archive
provided by the Asahi Linux installer into a manifest and CPIO and TAR
archives that are compatible with the Linux kernel.")
      (license license:expat))))
