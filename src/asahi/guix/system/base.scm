(define-module (asahi guix system base)
  #:use-module (asahi guix bootloader m1n1)
  #:use-module (asahi guix initrd)
  #:use-module (asahi guix packages linux)
  #:use-module (asahi guix services console-font)
  #:use-module (asahi guix services firmware)
  #:use-module (asahi guix services sound)
  #:use-module (asahi guix substitutes)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system nss)
  #:use-module (gnu system shadow)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (ice-9 optargs)
  #:export (asahi-operating-system
            asahi-edge-operating-system))

(define %kernel-arguments
  (append '("net.ifnames=0") %default-kernel-arguments))

(define %keyboard-options
  '("caps:ctrl_modifier"
    "terminate:ctrl_alt_bksp"))

(define %keyboard-layout
  (keyboard-layout "us" #:options %keyboard-options))

(define %file-systems
  (cons* (file-system
           (device (file-system-label "EFI - UEFI"))
           (mount-point "/boot/efi")
           (needed-for-boot? #t)
           (type "vfat"))
         (file-system
           (device (file-system-label "asahi-guix-root"))
           (mount-point "/")
           (needed-for-boot? #t)
           (type "ext4"))
         %base-file-systems))

(define %packages
  (cons* e2fsprogs network-manager %base-packages))

(define %openssh-configuration
  (openssh-configuration (openssh openssh-sans-x)))

(define %services
  (modify-services (cons* (service asahi-firmware-service-type)
                          (service network-manager-service-type)
                          (service openssh-service-type %openssh-configuration)
                          (service wpa-supplicant-service-type)
                          %base-services)
    (console-font-service-type config => (console-font-terminus config))
    (guix-service-type config => (append-substitutes config))))

(define %users
  (cons (user-account
         (name "guest")
         (comment "Guest")
         (group "users")
         (supplementary-groups '("audio" "netdev" "video" "wheel")))
        %base-user-accounts))

(define asahi-operating-system
  (operating-system
    (host-name "asahi-guix")
    (locale "en_US.utf8")
    (timezone "Europe/Berlin")
    (keyboard-layout %keyboard-layout)
    (bootloader (bootloader-configuration
                 (bootloader m1n1-u-boot-grub-bootloader)
                 (targets (list "/boot/efi"))
                 (keyboard-layout %keyboard-layout)))
    (kernel asahi-linux-edge)
    (kernel-arguments %kernel-arguments)
    (initrd-modules asahi-initrd-modules-edge)
    (file-systems %file-systems)
    (packages %packages)
    (services %services)
    (users %users)))

(define asahi-edge-operating-system
  (operating-system
    (inherit asahi-operating-system)
    (kernel asahi-linux-edge)
    (initrd-modules asahi-initrd-modules-edge)))
