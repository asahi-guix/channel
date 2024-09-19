(define-module (asahi guix manifests)
  #:use-module (asahi guix images installer)
  #:use-module (asahi guix packages)
  #:use-module (asahi guix systems base)
  #:use-module (asahi guix systems edge)
  #:use-module (asahi guix systems gnome)
  #:use-module (asahi guix systems install)
  #:use-module (asahi guix systems plasma)
  #:use-module (asahi guix systems sway)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix transformations)
  #:export (%asahi-image-manifest
            %asahi-package-manifest
            %asahi-system-manifest))

(define %version "0.1.0")

;; Images

(define %asahi-installer-image
  (manifest-entry
    (name "asahi-installer-image")
    (version %version)
    (item asahi-installer-image)))

(define %asahi-image-manifest
  (manifest (list %asahi-installer-image)))

;; Systems

(define %asahi-guix-base
  (manifest-entry
    (name "asahi-guix-base")
    (version %version)
    (item asahi-base-os)))

(define %asahi-guix-edge
  (manifest-entry
    (name "asahi-guix-edge")
    (version %version)
    (item asahi-edge-os)))

(define %asahi-guix-gnome
  (manifest-entry
    (name "asahi-guix-gnome")
    (version %version)
    (item asahi-gnome-os)))

(define %asahi-guix-installer
  (manifest-entry
    (name "asahi-guix-installer")
    (version %version)
    (item asahi-installation-os)))

(define %asahi-guix-plasma
  (manifest-entry
    (name "asahi-guix-plasma")
    (version %version)
    (item asahi-plasma-os)))

(define %asahi-guix-sway
  (manifest-entry
    (name "asahi-guix-sway")
    (version %version)
    (item asahi-sway-os)))

(define %asahi-system-manifest
  (manifest (list %asahi-guix-base
                  %asahi-guix-edge
                  %asahi-guix-gnome
                  ;; %asahi-guix-installer
                  %asahi-guix-plasma
                  %asahi-guix-sway)))

;; Packages

(define %asahi-package-manifest
  (packages->manifest (asahi-packages)))

(concatenate-manifests
 (list %asahi-image-manifest
       %asahi-package-manifest
       %asahi-system-manifest))
