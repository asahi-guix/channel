(define-module (gnu system images asahi installer)
  #:use-module (asahi guix build bootloader m1n1)
  #:use-module (asahi guix build modules)
  #:use-module (gnu image)
  #:use-module (gnu packages guile)
  #:use-module (gnu system image)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix platforms arm)
  #:export (asahi-installer-image-type
            make-installer-image))

(define asahi-efi-partition
  (partition
   (size (* 500 (expt 2 20)))
   (offset root-offset)
   (label "EFI")
   (file-system "vfat")
   (file-system-options (list "-S" "4096"))
   (flags '(esp))
   (initializer (with-extensions (list guile-zlib)
                  (with-imported-modules (source-module-closure
                                          '((asahi guix build bootloader m1n1))
                                          #:select? import-asahi-module?)
                    #~(lambda* (root . args)
                        (use-modules (asahi guix build bootloader m1n1))
                        (apply m1n1-initialize-efi-partition root args)))))))

(define asahi-root-partition
  (partition
   (size 'guess)
   (label root-label)
   (file-system "btrfs")
   (file-system-options (list "-s" "4096"))
   (flags '(boot))
   (uuid "fef23143-fe46-4f7f-bbb9-efc46a2a5e48")
   (initializer (gexp initialize-root-partition))))

(define asahi-installer-image-type
  (image-type
   (name 'asahi-installer)
   (constructor
    (lambda (os)
      (image
       (operating-system os)
       (format 'disk-image)
       (partition-table-type 'gpt)
       (partitions (list asahi-efi-partition asahi-root-partition))
       (volatile-root? #f))))))

(define (make-installer-image name os)
  (image
   (inherit
    (os+platform->image os aarch64-linux #:type asahi-installer-image-type))
   (name name)))
