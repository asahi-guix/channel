(define-module (asahi guix systems install)
  #:use-module (asahi guix channels)
  #:use-module (asahi guix config)
  #:use-module (asahi guix initrd)
  #:use-module (asahi guix packages linux)
  #:use-module (asahi guix services console-font)
  #:use-module (asahi guix services firmware)
  #:use-module (asahi guix substitutes)
  #:use-module (asahi guix systems base)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services base)
  #:use-module (gnu services)
  #:use-module (gnu system install)
  #:use-module (gnu system))

(define %services
  (modify-services (cons (service asahi-firmware-service-type)
                         (operating-system-user-services installation-os))
    (console-font-service-type
     config => (console-font-terminus config))
    (guix-service-type
     config => (guix-configuration
                (inherit config)
                (authorized-keys
                 (append %asahi-substitute-keys
                         (guix-configuration-authorized-keys config)))
                (channels asahi-channels)
                (guix (guix-for-channels asahi-channels))
                (substitute-urls
                 (append %asahi-substitute-urls
                         (guix-configuration-substitute-urls config)))))))

(define-public asahi-installation-os
  (operating-system
    (inherit installation-os)
    (kernel asahi-linux)
    (kernel-arguments '("debug"))
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/dev/sda"))))
    (initrd-modules asahi-initrd-modules)
    (services %services)))

asahi-installation-os
