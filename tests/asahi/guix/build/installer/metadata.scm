(define-module (tests asahi guix build installer metadata)
  #:use-module (asahi guix build installer metadata)
  #:use-module (asahi guix build installer os)
  #:use-module (asahi guix build installer partition)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define suite "test-asahi-guix-build-installer")

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

(define edge-os
  (installer-os
   (boot-object "m1n1.bin")
   (default-os-name "Asahi Guix Edge")
   (extras '())
   (icon "/gnu/store/iwzvbzdvby9mr4xm9znw0c1ll5m1gbz5-asahi-installer-edge-0.0.1/share/asahi-installer/os/asahi-edge-image.icns")
   (name "Asahi Guix Edge v1.4.0-25.e85f52e")
   (next-object "m1n1/boot.bin")
   (package "/gnu/store/iwzvbzdvby9mr4xm9znw0c1ll5m1gbz5-asahi-installer-edge-0.0.1/share/asahi-installer/os/asahi-edge-image.zip")
   (partitions (list efi-partition root-partition))
   (supported-fw '("12.3" "12.3.1" "13.5"))))

(define test-installer-metadata
  (installer-metadata
   (os-list
    (list base-os))))

(test-begin suite)

(test-equal "merge installer data"
  (installer-metadata
   (os-list
    (list base-os edge-os)))
  (merge-installer-metadata
   (installer-metadata
    (os-list
     (list base-os)))
   (installer-metadata
    (os-list
     (list edge-os)))))

(test-equal "write and read installer data"
  test-installer-metadata
  (let ((filename "/tmp/asahi-guix/write/installer/data"))
    (write-installer-metadata test-installer-metadata filename)
    (read-installer-metadata filename)))

(test-end suite)
