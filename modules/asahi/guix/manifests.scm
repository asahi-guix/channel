(define-module (asahi guix manifests)
  #:use-module (asahi guix config)
  #:use-module (asahi guix images base)
  #:use-module (asahi guix images installer)
  #:use-module (asahi guix packages)
  #:use-module (asahi guix systems base)
  #:use-module (asahi guix systems gnome)
  #:use-module (asahi guix systems install)
  #:use-module (asahi guix systems plasma)
  #:use-module (asahi guix systems sway)
  #:use-module (gnu system)
  #:use-module (gnu system image)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix transformations)
  #:export (%asahi-images-manifest
            %asahi-packages-manifest
            %asahi-systems-manifest))

;; Images

(define %asahi-installer-base-image-entry
  (manifest-entry
    (name "asahi-installer-base-image")
    (version %asahi-version)
    (item (system-image asahi-base-os-image))))

(define %guix-installer-image-entry
  (manifest-entry
    (name "guix-installer-image")
    (version %asahi-version)
    (item (system-image asahi-installer-os-image))))

(define %asahi-images-manifest
  (manifest (list %asahi-installer-base-image-entry
                  %guix-installer-image-entry)))

;; Systems

(define %asahi-base-entry
  (manifest-entry
    (name "asahi-base-os")
    (version %asahi-version)
    (item asahi-base-os)))

(define %asahi-gnome-os-entry
  (manifest-entry
    (name "asahi-gnome-os")
    (version %asahi-version)
    (item asahi-gnome-os)))

(define asahi-installer-os-entry
  (manifest-entry
    (name "asahi-installer-os")
    (version %asahi-version)
    (item asahi-installation-os)))

(define %asahi-plasma-os-entry
  (manifest-entry
    (name "asahi-plasma-os")
    (version %asahi-version)
    (item asahi-plasma-os)))

(define %asahi-sway-os-entry
  (manifest-entry
    (name "asahi-sway-os")
    (version %asahi-version)
    (item asahi-sway-os)))

(define %asahi-systems-manifest
  (manifest (list %asahi-base-entry
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
