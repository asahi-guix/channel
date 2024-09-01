(use-modules ;; (asahi guix initrd)
             ;; (asahi guix packages linux)
             (asahi guix systems base)
             ;; (asahi guix systems install)
             (gnu packages base)
             (gnu system)
             (guix gexp)
             (gnu packages base)
             (guix packages)
             (guix profiles)
             (guix transformations))

(define %asahi-guix-version "0.1.0")

(define %asahi-guix-base
  (manifest-entry
    (name "asahi-guix-base")
    (version %asahi-guix-version)
    (item asahi-base-os)))

;; (define %asahi-guix-edge
;;   (manifest-entry
;;     (name "asahi-guix-edge")
;;     (version %asahi-guix-version)
;;     (item asahi-edge-os)))

;; (define %asahi-guix-installer
;;   (manifest-entry
;;     (name "asahi-guix-installer")
;;     (version %asahi-guix-version)
;;     (item asahi-installation-os)))

;; (concatenate-manifests
;;  (list (manifest
;;         (list %asahi-guix-base
;;               %asahi-guix-edge
;;               %asahi-guix-installer))))

;; (packages->manifest (specifications->packages '("hello")))

(concatenate-manifests
 (list (packages->manifest (list hello which))
       (manifest (list %asahi-guix-server))))
