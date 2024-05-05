(use-modules (asahi guix initrd)
             (asahi guix packages linux)
             (asahi guix system base)
             (asahi guix system install)
             (gnu packages base)
             (gnu system)
             (guix gexp)
             (guix packages)
             (gnu packages base)
             (guix profiles))

(define %asahi-guix-version "0.1")

(define %asahi-guix-system
  (manifest-entry
    (name "asahi-guix-system")
    (version %asahi-guix-version)
    (item (asahi-operating-system
           #:esp-uuid "41F0-16FF"))))

(define %asahi-guix-system-edge
  (manifest-entry
    (name "asahi-guix-system-edge")
    (version %asahi-guix-version)
    (item (asahi-operating-system
           #:esp-uuid "41F0-16FF"
           #:initrd-modules asahi-initrd-modules-edge
           #:kernel asahi-linux-edge))))

(define %asahi-guix-installer
  (manifest-entry
    (name "asahi-guix-installer")
    (version %asahi-guix-version)
    (item asahi-installation-operating-system)))

(concatenate-manifests
 (list (manifest
        (list %asahi-guix-system
              %asahi-guix-system-edge
              %asahi-guix-installer))))
