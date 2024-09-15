(define-module (asahi guix images installer)
  #:use-module (asahi guix systems base)
  #:use-module (gnu build image)
  #:use-module (gnu image)
  #:use-module (gnu system image)
  #:use-module (guix gexp)
  #:use-module (guix platforms arm)
  #:export (asahi-installer-image
            asahi-installer-image-type))

(define asahi-installer-esp-partition
  (partition
   (size (* 40 (expt 2 20)))
   (offset root-offset)
   (label "BOOT")
   (file-system "vfat")
   (flags '(esp))
   (initializer (gexp initialize-efi-partition))))

(define asahi-installer-root-partition
  (partition
   (size 'guess)
   (label root-label)
   (file-system "ext4")
   (file-system-options (list "-O" "^metadata_csum,^64bit"))
   (flags '(boot))
   (uuid "fef23143-fe46-4f7f-bbb9-efc46a2a5e48")
   (initializer (gexp initialize-root-partition))))

(define asahi-installer-image-type
  (image-type
   (name 'asahi-base-raw)
   (constructor
    (lambda (os)
      (image
       (operating-system os)
       (format 'disk-image)
       (partition-table-type 'gpt)
       (partitions (list asahi-installer-esp-partition
                         asahi-installer-root-partition)))))))

(define asahi-installer-image
  (image
   (inherit
    (os+platform->image
     asahi-base-os
     aarch64-linux
     #:type asahi-installer-image-type))
   (name 'asahi-installer-image)))

asahi-installer-image
