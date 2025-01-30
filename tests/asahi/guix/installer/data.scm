(define-module (tests asahi guix installer data)
  #:use-module (asahi guix installer data)
  #:use-module (asahi guix installer os)
  #:use-module (asahi guix installer partition)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define suite "test-asahi-guix-installer-data")

(define efi-partition
  (installer-partition
   (copy-installer-metadata? #t)
   (copy-firmware? #t)
   (expand? #f)
   (format "fat")
   (image "GNU-ESP.img")
   (name "GNU-ESP")
   (size "41943040B")
   (source "esp")
   (type "EFI")
   (volume-id "0x8f96cafc")))

(define root-partition
  (installer-partition
   (copy-installer-metadata? #f)
   (copy-firmware? #f)
   (expand? #t)
   (format #f)
   (image "Guix_image.img")
   (name "Guix_image")
   (size "2103726080B")
   (source #f)
   (type "Linux")
   (volume-id #f)))

(define base-os
  (installer-os
   (boot-object "m1n1.bin")
   (default-os-name "Asahi Guix Base")
   (extras '())
   (icon "/gnu/store/iwzvbzdvby9mr4xm9znw0c1ll5m1gbz5-asahi-installer-base-0.0.1/share/asahi-installer/os/asahi-base-image.icns")
   (name "Asahi Guix Base v1.4.0-25.e85f52e")
   (next-object "m1n1/boot.bin")
   (package "/gnu/store/iwzvbzdvby9mr4xm9znw0c1ll5m1gbz5-asahi-installer-base-0.0.1/share/asahi-installer/os/asahi-base-image.zip")
   (partitions (list efi-partition root-partition))
   (supported-fw '("12.3" "12.3.1" "13.5"))))

(define sway-os
  (installer-os
   (boot-object "m1n1.bin")
   (default-os-name "Asahi Guix Sway")
   (extras '())
   (icon "/gnu/store/iwzvbzdvby9mr4xm9znw0c1ll5m1gbz5-asahi-installer-sway-0.0.1/share/asahi-installer/os/asahi-sway-image.icns")
   (name "Asahi Guix Sway v1.4.0-25.e85f52e")
   (next-object "m1n1/boot.bin")
   (package "/gnu/store/iwzvbzdvby9mr4xm9znw0c1ll5m1gbz5-asahi-installer-sway-0.0.1/share/asahi-installer/os/asahi-sway-image.zip")
   (partitions (list efi-partition root-partition))
   (supported-fw '("12.3" "12.3.1" "13.5"))))

(define test-installer-data
  (installer-data
   (os-list
    (list base-os))))

(test-begin suite)

(test-equal "merge installer data"
  (installer-data
   (os-list
    (list base-os sway-os)))
  (merge-installer-data
   (installer-data
    (os-list
     (list base-os)))
   (installer-data
    (os-list
     (list sway-os)))))

(test-equal "write and read installer data"
  test-installer-data
  (let ((filename "/tmp/asahi-guix/write/installer/data"))
    (write-installer-data test-installer-data filename)
    (read-installer-data filename)))

(test-equal "replace installer data package substring"
  (installer-data
   (os-list
    (list (installer-os
           (inherit base-os)
           (package "abc")))))
  (installer-data-apply-package test-installer-data (lambda (package) "abc")))

(test-end suite)
