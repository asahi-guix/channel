(define-module (asahi guix build installer)
  #:use-module (asahi guix build sfdisk)
  #:use-module (asahi guix build utils)
  #:use-module (guix build utils)
  #:use-module (guix records)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:export (installer
            installer-disk-images
            installer-icon
            installer-os
            installer-os-boot-object
            installer-os-default-os-name
            installer-os-extras
            installer-os-icon
            installer-os-name
            installer-os-next-object
            installer-os-package
            installer-os-partitions
            installer-os-supported-fw
            installer-os?
            installer-output-dir
            installer-package-version
            installer-data
            installer-data-os-list
            installer-data?
            installer-partition
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
            installer-work-dir
            installer?
            make-asahi-installer-package
            make-asahi-installer-package-main
            make-installer
            make-installer-data
            make-installer-os
            make-installer-partition
            write-installer-data
            read-installer-data))

(define %output-dir "/tmp/asahi-guix/installer/out")
(define %package-version "1.4.0")
(define %work-dir "/tmp/asahi-guix/installer/work")

(define %supported-firmwares
  (list "12.3" "12.3.1" "13.5"))

(define %os-names
  '(("asahi-base-image" . "Asahi Guix Base")
    ("asahi-edge-image" . "Asahi Guix Edge")
    ("asahi-gnome-image" . "Asahi Guix with Gnome")
    ("asahi-plasma-image" . "Asahi Guix with KDE Plasma")
    ("asahi-sway-image" . "Asahi Guix with Sway")))

(define-record-type* <installer>
  installer
  make-installer
  installer?
  (disk-images installer-disk-images)
  (icon installer-icon (default #f))
  (output-dir installer-output-dir (default %output-dir))
  (package-version installer-package-version (default %package-version))
  (work-dir installer-work-dir (default %work-dir)))

(define-record-type* <installer-data>
  installer-data
  make-installer-data
  installer-data?
  (os-list installer-data-os-list))

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

(define-record-type* <installer-partition>
  installer-partition
  make-installer-partition
  installer-partition?
  (copy-installer-data? installer-partition-copy-installer-data? (default #f))
  (copy-firmware? installer-partition-copy-firmware? (default #f))
  (expand? installer-partition-expand? (default #f))
  (format installer-partition-format (default #f))
  (image installer-partition-image)
  (name installer-partition-name)
  (size installer-partition-size)
  (source installer-partition-source (default #f))
  (type installer-partition-type)
  (volume-id installer-partition-volume-id (default #f)))

(define (installer-partition->json partition)
  `(("copy_firmware" . ,(installer-partition-copy-firmware? partition))
    ("copy_installer_data" . ,(installer-partition-copy-installer-data? partition))
    ("expand" . ,(installer-partition-expand? partition))
    ("format" . ,(or (installer-partition-format partition) 'null))
    ("image" . ,(installer-partition-image partition))
    ("name" . ,(installer-partition-name partition))
    ("size" . ,(installer-partition-size partition))
    ("source" . ,(or (installer-partition-source partition) 'null))
    ("type" . ,(installer-partition-type partition))
    ("volume_id" . ,(or (installer-partition-volume-id partition) 'null))))

(define (null->false x)
  (if (eq? 'null x) #f x))

(define (json->installer-partition alist)
  (installer-partition
   (copy-firmware? (assoc-ref alist "copy_firmware"))
   (copy-installer-data? (assoc-ref alist "copy_installer_data"))
   (expand? (assoc-ref alist "expand"))
   (format (null->false (assoc-ref alist "format")))
   (image (assoc-ref alist "image"))
   (name (assoc-ref alist "name"))
   (size (assoc-ref alist "size"))
   (source (null->false (assoc-ref alist "source")))
   (type (assoc-ref alist "type"))
   (volume-id (null->false (assoc-ref alist "volume_id")))))

(define (installer-os->json os)
  (define partitions
    (map (lambda (partition)
           (installer-partition->json partition))
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

(define (json->installer-os alist)
  (installer-os
   (boot-object (assoc-ref alist "boot_object"))
   (default-os-name (assoc-ref alist "default_os_name"))
   (extras (vector->list (assoc-ref alist "extras")))
   (icon (null->false (assoc-ref alist "icon")))
   (name (assoc-ref alist "name"))
   (next-object (assoc-ref alist "next_object"))
   (package (assoc-ref alist "package"))
   (partitions (map json->installer-partition
                    (vector->list (assoc-ref alist "partitions"))))
   (supported-fw (vector->list (assoc-ref alist "supported_fw")))))

(define (installer-data->json data)
  (define os-list
    (map (lambda (os)
           (installer-os->json os))
         (installer-data-os-list data)))
  `(("os_list" . ,(apply vector os-list))))

(define (json->installer-data alist)
  (installer-data
   (os-list
    (map json->installer-os
         (vector->list (assoc-ref alist "os_list"))))))

(define (string-blank? s)
  (string-match "^\\s*$" s))

(define (disk-image-name filename)
  (let ((parts (string-split filename #\/)))
    (string-join (cdr (string-split (last parts) #\-)) "-")))

(define (os-short-name disk-image)
  (assoc-ref %os-names (disk-image-name disk-image)))

(define (os-long-name installer disk-image)
  (string-replace-substring
   (os-short-name disk-image)
   "Asahi Guix"
   (format #f "Asahi Guix ~a" (installer-package-version installer))))

(define (installer-esp-dir installer)
  (format #f "~a/package/esp" (installer-work-dir installer)))

(define (installer-package-name installer disk-image)
  (format #f "~a/~a.zip"
          (installer-output-dir installer)
          (disk-image-name disk-image)))

(define (parse-serial-number text)
  (let ((match (string-match "serial number\\s+(0x[0-9a-fA-F]+)" text)))
    (if (regexp-match? match)
        (match:substring match 1)
        #f)))

(define (installer-esp-volume-id installer partition)
  (let ((filename (installer-partition-filename installer partition)))
    (when (file-exists? filename)
      (parse-serial-number (command-output "file" filename)))))

(define (installer-data-filename installer)
  (format #f "~a/installer_data.json" (installer-output-dir installer)))

(define (installer-partition-filename installer partition)
  (format #f "~a/package/~a.img"
          (installer-work-dir installer)
          (sfdisk-partition-name partition)))

(define (efi-partition? partition)
  (equal? "C12A7328-F81F-11D2-BA4B-00A0C93EC93B"
          (sfdisk-partition-type partition)))

(define (linux-partition? partition)
  (equal? "0FC63DAF-8483-4772-8E79-3D69D8477DE4"
          (sfdisk-partition-type partition)))

(define (partition-index table partition)
  (list-index (lambda (p)
                (equal? partition p))
              (sfdisk-table-partitions table)))

(define (partition-filename installer table partition)
  (format #f "~a/~a" (installer-work-dir installer)
          (list-index (lambda (p)
                        (equal? partition p))
                      (sfdisk-table-partitions table))))

(define (extract-command installer table partition)
  (let ((work-dir (installer-work-dir installer)))
    (list "dd"
          (format #f "if=~a" (sfdisk-table-device table))
          (format #f "of=~a" (installer-partition-filename installer partition))
          (format #f "skip=~a" (sfdisk-partition-start partition))
          (format #f "count=~a" (sfdisk-partition-size partition))
          (format #f "bs=~a" (sfdisk-table-sector-size table)))))

(define (extract-partition installer table partition)
  (let ((command (extract-command installer table partition))
        (filename (installer-partition-filename installer partition)))
    (mkdir-p (dirname filename))
    (apply system* command)
    filename))

(define (partition-size filename)
  (format #f "~aB" (stat:size (stat filename))))

(define (unpack-efi-partition installer partition)
  (let ((directory (installer-esp-dir installer))
        (filename (installer-partition-filename installer partition)))
    (mkdir-p directory)
    (invoke "7z" "-aoa" "x" (format #f "-o~a" directory) filename)))

(define (build-efi-partition installer table partition)
  (let ((filename (extract-partition installer table partition))
        (work-dir (installer-work-dir installer)))
    (format #t "  Partition #~a: ~a\n" (partition-index table partition) filename)
    (unpack-efi-partition installer partition)
    (installer-partition
     (copy-firmware? #t)
     (copy-installer-data? #t)
     (format "fat")
     (image (basename filename))
     (name (sfdisk-partition-name partition))
     (size (partition-size filename))
     (source "esp")
     (type "EFI")
     (volume-id (installer-esp-volume-id installer partition)))))

(define (build-linux-partition installer table partition)
  (let ((filename (extract-partition installer table partition)))
    (format #t "  Partition #~a: ~a\n" (partition-index table partition) filename)
    (installer-partition
     (expand? #t)
     (image (basename filename))
     (name (sfdisk-partition-name partition))
     (size (partition-size filename))
     (type "Linux"))))

(define (build-partition installer table partition)
  (cond ((efi-partition? partition)
         (build-efi-partition installer table partition))
        ((linux-partition? partition)
         (build-linux-partition installer table partition))))

(define (build-partitions installer table)
  (map (lambda (partition)
         (build-partition installer table partition))
       (sfdisk-table-partitions table)))

(define (build-package-archive installer disk-image)
  (let* ((archive-name (installer-package-name installer disk-image))
         (package-dir (format #f "~a/package" (installer-work-dir installer))))
    (with-directory-excursion package-dir
      (invoke "7z" "a" "-tzip" "-r" archive-name))))

(define* (build-os installer disk-image)
  (let ((table (sfdisk-list disk-image))
        (name (os-long-name installer disk-image)))
    (format #t "Building ~a ...\n" name)
    (let ((os (installer-os
               (default-os-name (os-short-name disk-image))
               (icon (installer-icon installer))
               (name name)
               (package (installer-package-name installer disk-image))
               (partitions (build-partitions installer table)))))
      (build-package-archive installer disk-image)
      os)))

(define (build-installer-data installer)
  (installer-data
   (os-list (map (lambda (disk-image)
                   (build-os installer disk-image))
                 (installer-disk-images installer)))))

(define (read-installer-data filename)
  (call-with-input-file filename
    (lambda (port)
      (json->installer-data (json->scm port)))))

(define (write-installer-data data filename)
  (let ((content (scm->json-string (installer-data->json data) #:pretty #t)))
    (mkdir-p (dirname filename))
    (call-with-output-file filename
      (lambda (port)
        (set-port-encoding! port "UTF-8")
        (format port "~a\n" content)))
    data))

(define (save-installer-data installer data)
  (let ((filename (installer-data-filename installer)))
    (write-installer-data data filename)))

(define* (make-asahi-installer-package
          disk-images #:key
          (icon #f)
          (output-dir %output-dir)
          (package-version %package-version)
          (work-dir %work-dir))
  (format #t "Building Asahi Guix installer packages ...\n")
  (let* ((installer (installer
                     (disk-images disk-images)
                     (icon icon)
                     (output-dir output-dir)
                     (package-version package-version)))
         (data (build-installer-data installer)))
    (save-installer-data installer data)
    data))

;; Getopt

(define option-spec
  '((help (single-char #\h) (value #f))
    (icon (single-char #\i) (value #t))
    (output-dir (single-char #\o) (value #t))
    (package-version (single-char #\p) (value #t))
    (work-dir (single-char #\w) (value #t))))

(define (icon-option options)
  (option-ref options 'icon #f))

(define (output-dir-option options)
  (option-ref options 'output-dir %output-dir))

(define (package-version-option options)
  (option-ref options 'package-version %package-version))

(define (work-dir-option options)
  (option-ref options 'work-dir %work-dir))

(define (show-usage)
  (display "Usage: make-asahi-installer-package [options] DISK-IMAGE\n\n")
  (display "Options:\n")
  (display "  -h, --help                      show this help\n")
  (display "  -i, --icon=ICON                 the icon to use\n")
  (display "  -o, --output-dir=DIR            the output directory\n")
  (display "  -p, --package-version=VERSION   the package version to use\n")
  (display "  -w, --work-dir=DIR              the working directory\n"))

(define* (make-asahi-installer-package-main args)
  (let* ((options (getopt-long args option-spec))
         (disk-images (option-ref options '() #f)))
    (if (or (option-ref options 'help #f)
            (null? args))
        (show-usage)
        (make-asahi-installer-package
         disk-images
         #:icon (icon-option options)
         #:output-dir (output-dir-option options)
         #:package-version (package-version-option options)
         #:work-dir (work-dir-option options)))))

;; (define my-data (make-asahi-installer-package (list "/gnu/store/hfr97d38hpgq2skh10192f1ik1smvrx7-asahi-base-image")))
