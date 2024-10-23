(define-module (asahi guix systems base)
  #:use-module (asahi guix build utils)
  #:use-module (asahi guix initrd)
  #:use-module (asahi guix packages linux)
  #:use-module (asahi guix services console-font)
  #:use-module (asahi guix services firmware)
  #:use-module (asahi guix services sound)
  #:use-module (asahi guix substitutes)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader m1n1)
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
  #:use-module (gnu system image)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system nss)
  #:use-module (gnu system shadow)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (ice-9 optargs)
  #:export (asahi-base-os))

(define %asahi-keyboard-options
  '("caps:ctrl_modifier" "terminate:ctrl_alt_bksp"))

(define %asahi-keyboard-layout
  (keyboard-layout "us" #:options %asahi-keyboard-options))

(define %asahi-base-bootloader
  (bootloader-configuration
   (bootloader m1n1-u-boot-grub-bootloader)
   (targets (list "/boot/efi"))
   (keyboard-layout %asahi-keyboard-layout)))

(define %asahi-guest-user
  (user-account
   (name "guest")
   (comment "Guest")
   (group "users")
   (supplementary-groups '("audio" "netdev" "video" "wheel"))))

(define %asahi-default-kernel-arguments
  (append '("net.ifnames=0") %default-kernel-arguments))

(define %asahi-base-file-system-boot
  (file-system
    (device (file-system-label (escape-label "EFI - ASAHI")))
    (mount-point "/boot/efi")
    (needed-for-boot? #t)
    (type "fat32")))

(define %asahi-base-file-system-root
  (file-system
    (device (file-system-label (escape-label root-label)))
    (mount-point "/")
    (needed-for-boot? #t)
    (type "btrfs")))

(define %asahi-base-file-systems
  (cons* %asahi-base-file-system-boot
         %asahi-base-file-system-root
         %base-file-systems))

(define %asahi-base-packages
  (cons* e2fsprogs network-manager %base-packages))

(define %asahi-base-openssh-configuration
  (openssh-configuration (openssh openssh-sans-x)))

(define %asahi-base-services
  (modify-services (cons* (service asahi-firmware-service-type)
                          (service network-manager-service-type)
                          (service openssh-service-type %asahi-base-openssh-configuration)
                          (service wpa-supplicant-service-type)
                          %base-services)
    (console-font-service-type config => (console-font-terminus config))
    (guix-service-type config => (append-substitutes config))))

(define %asahi-base-user-accounts
  (cons %asahi-guest-user %base-user-accounts))

(define asahi-base-os
  (operating-system
    (host-name "asahi-guix")
    (locale "en_US.utf8")
    (timezone "Europe/Berlin")
    (keyboard-layout %asahi-keyboard-layout)
    (bootloader %asahi-base-bootloader)
    (kernel asahi-linux)
    (kernel-arguments %asahi-default-kernel-arguments)
    (initrd-modules asahi-initrd-modules)
    (file-systems %asahi-base-file-systems)
    (packages %asahi-base-packages)
    (services %asahi-base-services)
    (users %asahi-base-user-accounts)))

asahi-base-os
