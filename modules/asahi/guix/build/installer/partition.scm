(define-module (asahi guix build installer partition)
  #:use-module (asahi guix build utils)
  #:use-module (guix records)
  #:export (installer-partition
            installer-partition->json-alist
            installer-partition-copy-firmware?
            installer-partition-copy-installer-data?
            installer-partition-expand?
            installer-partition-format
            installer-partition-image
            installer-partition-name
            installer-partition-size
            installer-partition-source
            installer-partition-type
            installer-partition-volume-id
            installer-partition?
            json-alist->installer-partition
            make-installer-partition))

(define-record-type* <installer-partition>
  installer-partition
  make-installer-partition
  installer-partition?
  (copy-installer-data? installer-partition-copy-installer-data? (default #f))
  (copy-firmware? installer-partition-copy-firmware? (default #f))
  (expand? installer-partition-expand? (default #f))
  (format installer-partition-format (default #f))
  (image installer-partition-image (default #f))
  (name installer-partition-name)
  (size installer-partition-size)
  (source installer-partition-source (default #f))
  (type installer-partition-type)
  (volume-id installer-partition-volume-id (default #f)))

(define (installer-partition->json-alist partition)
  "Convert an installer partition to a JSON alist."
  `(("copy_firmware" . ,(installer-partition-copy-firmware? partition))
    ("copy_installer_data" . ,(installer-partition-copy-installer-data? partition))
    ("expand" . ,(installer-partition-expand? partition))
    ("format" . ,(or (installer-partition-format partition) 'null))
    ("image" . ,(or (installer-partition-image partition) 'null))
    ("name" . ,(installer-partition-name partition))
    ("size" . ,(installer-partition-size partition))
    ("source" . ,(or (installer-partition-source partition) 'null))
    ("type" . ,(installer-partition-type partition))
    ("volume_id" . ,(or (installer-partition-volume-id partition) 'null))))

(define (json-alist->installer-partition alist)
  "Convert a JSON alist to an installer partition."
  (installer-partition
   (copy-firmware? (assoc-ref alist "copy_firmware"))
   (copy-installer-data? (assoc-ref alist "copy_installer_data"))
   (expand? (assoc-ref alist "expand"))
   (format (null->false (assoc-ref alist "format")))
   (image (null->false (assoc-ref alist "image")))
   (name (assoc-ref alist "name"))
   (size (assoc-ref alist "size"))
   (source (null->false (assoc-ref alist "source")))
   (type (assoc-ref alist "type"))
   (volume-id (null->false (assoc-ref alist "volume_id")))))
