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
  #:export (%asahi-images-manifest
            %asahi-packages-manifest
            %asahi-systems-manifest))

(define %version "0.1.0")

;; Images

(define %asahi-installer-image-entry
  (manifest-entry
    (name "asahi-guix-installer-image")
    (version %version)
    (item asahi-guix-installer-image)))

(define %asahi-images-manifest
  (manifest (list %asahi-installer-image-entry)))

;; Systems

(define %asahi-base-entry
  (manifest-entry
    (name "asahi-base-os")
    (version %version)
    (item asahi-base-os)))

(define %asahi-edge-entry
  (manifest-entry
    (name "asahi-edge-os")
    (version %version)
    (item asahi-edge-os)))

(define %asahi-gnome-os-entry
  (manifest-entry
    (name "asahi-gnome-os")
    (version %version)
    (item asahi-gnome-os)))

(define asahi-installer-os-entry
  (manifest-entry
    (name "asahi-installer-os")
    (version %version)
    (item asahi-installation-os)))

(define %asahi-plasma-os-entry
  (manifest-entry
    (name "asahi-plasma-os")
    (version %version)
    (item asahi-plasma-os)))

(define %asahi-sway-os-entry
  (manifest-entry
    (name "asahi-sway-os")
    (version %version)
    (item asahi-sway-os)))

(define %asahi-systems-manifest
  (manifest (list %asahi-base-entry
                  %asahi-edge-entry
                  %asahi-gnome-os-entry
                  ;; asahi-installer-os-entry
                  %asahi-plasma-os-entry
                  %asahi-sway-os-entry)))

;; Packages

(define %asahi-packages-manifest
  (packages->manifest (asahi-packages)))

(concatenate-manifests
 (list %asahi-images-manifest
       %asahi-packages-manifest
       %asahi-systems-manifest))
