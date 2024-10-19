(define-module (gnu bootloader m1n1)
  #:use-module (asahi guix build bootloader m1n1)
  #:use-module (asahi guix build modules)
  #:use-module (asahi guix packages bootloader)
  #:use-module (asahi guix packages linux)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages guile)
  #:use-module (guix gexp)
  #:use-module (ice-9 pretty-print)
  #:use-module (guix modules))

(define m1n1-u-boot-grub-installer
  ;; TODO: guile-sqlite3 seems t be needed since 01-10-2024 ?
  (with-extensions (list guile-zlib guile-sqlite3)
    (with-imported-modules (source-module-closure
                            '((asahi guix build bootloader m1n1))
                            #:select? import-asahi-module?)
      #~(lambda (bootloader efi-dir mount-point)
          (use-modules (asahi guix build bootloader m1n1))
          (install-m1n1-u-boot-grub bootloader efi-dir mount-point)))))

(define-public m1n1-u-boot-grub-bootloader
  (efi-bootloader-chain
   grub-efi-removable-bootloader
   #:installer m1n1-u-boot-grub-installer
   #:packages (list asahi-linux asahi-m1n1 asahi-u-boot)))
