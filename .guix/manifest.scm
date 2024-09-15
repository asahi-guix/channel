(use-modules (asahi guix images installer)
             (asahi guix systems base)
             (asahi guix systems desktop)
             (asahi guix systems install)
             (gnu packages librewolf)
             (gnu system)
             (guix gexp)
             (guix packages)
             (guix profiles)
             (guix transformations))

(define %asahi-guix-version "0.1.0")

;; Images

(define %asahi-installer-image
  (manifest-entry
    (name "asahi-installer-image")
    (version %asahi-guix-version)
    (item asahi-installer-image)))

(define %asahi-images
  (manifest (list %asahi-installer-image)))

;; Systems

(define %asahi-guix-base
  (manifest-entry
    (name "asahi-guix-base")
    (version %asahi-guix-version)
    (item asahi-base-os)))

(define %asahi-guix-edge
  (manifest-entry
    (name "asahi-guix-edge")
    (version %asahi-guix-version)
    (item asahi-edge-os)))

(define %asahi-guix-gnome
  (manifest-entry
    (name "asahi-guix-gnome")
    (version %asahi-guix-version)
    (item asahi-gnome-os)))

(define %asahi-guix-installer
  (manifest-entry
    (name "asahi-guix-installer")
    (version %asahi-guix-version)
    (item asahi-installation-os)))

(define %asahi-guix-plasma
  (manifest-entry
    (name "asahi-guix-plasma")
    (version %asahi-guix-version)
    (item asahi-plasma-os)))

(define %asahi-guix-sway
  (manifest-entry
    (name "asahi-guix-sway")
    (version %asahi-guix-version)
    (item asahi-sway-os)))

(define %asahi-systems
  (manifest (list %asahi-guix-base
                  %asahi-guix-edge
                  %asahi-guix-gnome
                  ;; %asahi-guix-installer
                  %asahi-guix-plasma
                  %asahi-guix-sway)))

;; Packages

(define %asahi-packages
  (packages->manifest (list)))

(concatenate-manifests
 (list ;; %asahi-images
       %asahi-packages
       %asahi-systems))
