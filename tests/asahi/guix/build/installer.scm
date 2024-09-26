(define-module (tests asahi guix build installer)
  #:use-module (asahi guix build installer)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define suite "test-asahi-guix-build-installer")

(define efi-partition
  (installer-partition
   (copy-installer-data? #t)
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
   (copy-installer-data? #f)
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
   (icon #f)
   (name "Asahi Guix 1.4.0 Base")
   (next-object "m1n1/boot.bin")
   (package "/tmp/asahi-guix/installer/out/asahi-base-image.zip")
   (partitions (list efi-partition root-partition))
   (supported-fw '("12.3" "12.3.1" "13.5"))))

(define edge-os
  (installer-os
   (boot-object "m1n1.bin")
   (default-os-name "Asahi Guix Edge")
   (extras '())
   (icon #f)
   (name "Asahi Guix 1.4.0 Edge")
   (next-object "m1n1/boot.bin")
   (package "/tmp/asahi-guix/installer/out/asahi-edge-image.zip")
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
    (list base-os edge-os)))
  (merge-installer-data
   (installer-data
    (os-list
     (list base-os)))
   (installer-data
    (os-list
     (list edge-os)))))

(test-equal "write and read installer data"
  test-installer-data
  (let ((filename "/tmp/asahi-guix/write/installer/data"))
    (write-installer-data test-installer-data filename)
    (read-installer-data filename)))

(test-end suite)
