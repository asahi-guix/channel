(define-module (asahi guix systems image)
  #:use-module (asahi guix systems base)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu image)
  #:use-module (gnu system image)
  #:use-module (gnu tests)
  #:use-module (guix platforms arm)
  #:use-module (guix gexp))

(define MiB (expt 2 20))

;; (define-public asahi-base-image
;;   (image
;;    (format 'disk-image)
;;    ;; (format 'iso9660)
;;    ;; (format 'tarball)
;;    (operating-system operating-system)
;;    (partition-table-type 'gpt)
;;    (partitions
;;     (list
;;      (partition
;;       (size (* 40 MiB))
;;       ;; (offset (* 1024 1024))
;;       (label "GNU-ESP")
;;       (file-system "vfat")
;;       (flags '(esp))
;;       (initializer (gexp initialize-efi-partition)))
;;      (partition
;;       (file-system "ext4")
;;       (flags '(boot))
;;       (initializer (gexp initialize-root-partition))
;;       (label root-label)
;;       (size 'guess))))))

(define asahi-image-type
  (image-type
   (name 'asahi-base-raw)
   (constructor (lambda (os)
                  (image
                   ;; (inherit (raw-with-offset-disk-image))
                   (inherit efi-disk-image)
                   (operating-system os)
                   (platform aarch64-linux))))))

(define asahi-base-image
  (image
   (inherit
    (os+platform->image
     asahi-base-os
     aarch64-linux
     #:type asahi-image-type))
   (name 'asahi-base-image)))

asahi-base-image
