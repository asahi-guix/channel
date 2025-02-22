(define-module (gnu bootloader m1n1)
  #:use-module (asahi guix build bootloader m1n1)
  #:use-module (asahi guix build modules)
  #:use-module (asahi guix packages bootloader)
  #:use-module (asahi guix packages linux)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages guile)
  #:use-module (guix gexp)
  #:use-module (guix modules))

(define m1n1-u-boot-grub-installer
  (with-extensions (list coreutils bash-minimal gzip guile-sqlite3)
    (with-imported-modules (source-module-closure
                            '((asahi guix build bootloader m1n1))
                            #:select? import-asahi-module?)
      #~(lambda (bootloader efi-dir mount-point)
          (use-modules (asahi guix build bootloader m1n1))
          (setenv "PATH" (string-join
                          (filter string?
                                  (list (getenv "PATH")
                                        (string-append #$bash-minimal "/bin")
                                        (string-append #$coreutils "/bin")
                                        (string-append #$gzip "/bin")))
                          ":"))
          (install-m1n1-u-boot-grub bootloader efi-dir mount-point)))))

(define-public m1n1-u-boot-grub-bootloader
  (efi-bootloader-chain
   grub-efi-removable-bootloader
   #:installer m1n1-u-boot-grub-installer
   #:packages (list asahi-linux asahi-m1n1 asahi-u-boot)))
