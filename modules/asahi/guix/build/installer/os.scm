(define-module (asahi guix build installer os)
  #:use-module (asahi guix build installer partition)
  #:use-module (asahi guix build utils)
  #:use-module (guix records)
  #:export (installer-os
            installer-os->json-alist
            installer-os-copy-firmware?
            installer-os-copy-installer-metadata?
            installer-os-expand?
            installer-os-format
            installer-os-image
            installer-os-name
            installer-os-size
            installer-os-source
            installer-os-type
            installer-os-volume-id
            installer-os?
            json-alist->installer-os
            make-installer-os))

(define %supported-firmwares
  (list "12.3" "12.3.1" "13.5"))

(define-record-type* <installer-os>
  installer-os
  make-installer-os
  installer-os?
  (boot-object installer-os-boot-object (default "m1n1.bin"))
  (default-os-name installer-os-default-os-name)
  (extras installer-os-extras (default '()))
  (icon installer-os-icon)
  (name installer-os-name)
  (next-object installer-os-next-object (default "m1n1/boot.bin"))
  (package installer-os-package)
  (partitions installer-os-partitions)
  (supported-fw installer-os-supported-fw (default %supported-firmwares)))

(define (installer-os->json-alist os)
  (define partitions
    (map (lambda (partition)
           (installer-partition->json-alist partition))
         (installer-os-partitions os)))
  `(("boot_object" . ,(installer-os-boot-object os))
    ("default_os_name" . ,(installer-os-default-os-name os))
    ("extras" . ,(apply vector (installer-os-extras os)))
    ("icon" . ,(or (installer-os-icon os) 'null))
    ("name" . ,(installer-os-name os))
    ("next_object" . ,(installer-os-next-object os))
    ("package" . ,(installer-os-package os))
    ("partitions" . ,(apply vector partitions))
    ("supported_fw" . ,(apply vector (installer-os-supported-fw os)))))

(define (json-alist->installer-os alist)
  (installer-os
   (boot-object (assoc-ref alist "boot_object"))
   (default-os-name (assoc-ref alist "default_os_name"))
   (extras (vector->list (assoc-ref alist "extras")))
   (icon (null->false (assoc-ref alist "icon")))
   (name (assoc-ref alist "name"))
   (next-object (assoc-ref alist "next_object"))
   (package (assoc-ref alist "package"))
   (partitions (map json-alist->installer-partition
                    (vector->list (assoc-ref alist "partitions"))))
   (supported-fw (vector->list (assoc-ref alist "supported_fw")))))
