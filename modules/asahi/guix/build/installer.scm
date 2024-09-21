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
  #:export (make-asahi-installer-package
            make-asahi-installer-package-main))

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
  (copy-installer-data?? installer-partition-copy-installer-data?? (default #f))
  (copy-firmware? installer-partition-copy-firmware? (default #f))
  (expand? installer-partition-expand? (default #f))
  (format installer-partition-format (default #f))
  (image installer-partition-image)
  (name installer-partition-name)
  (size installer-partition-size)
  (source installer-partition-source (default #f))
  (type installer-partition-type)
  (volume-id installer-partition-volume-id))

(define (installer-partition->alist partition)
  `(("copy_firmware" . ,(installer-partition-copy-firmware? partition))
    ("copy_installer_data" . ,(installer-partition-copy-installer-data?? partition))
    ("expand" . ,(installer-partition-expand? partition))
    ("format" . ,(installer-partition-format partition))
    ("image" . ,(installer-partition-image partition))
    ("name" . ,(installer-partition-name partition))
    ("size" . ,(installer-partition-size partition))
    ("source" . ,(installer-partition-source partition))
    ("type" . ,(installer-partition-type partition))
    ("volume_id" . ,(installer-partition-volume-id partition))))

(define (installer-os->alist os)
  (define partitions
    (map (lambda (partition)
           (installer-partition->alist partition))
         (installer-os-partitions os)))
  `(("boot_object" . ,(installer-os-boot-object os))
    ("default_os_name" . ,(installer-os-default-os-name os))
    ("extras" . ,(apply vector (installer-os-extras os)))
    ("icon" . ,(installer-os-icon os))
    ("name" . ,(installer-os-name os))
    ("next_object" . ,(installer-os-next-object os))
    ("package" . ,(installer-os-package os))
    ("partitions" . ,(apply vector partitions))
    ("supported_fw" . ,(apply vector (installer-os-supported-fw os)))))

(define (installer-data->alist data)
  (define os-list
    (map (lambda (os)
           (installer-os->alist os))
         (installer-data-os-list data)))
  `(("os_list" . ,(apply vector os-list))))

;; (scm->json-string (installer-data->alist my-data))

(define option-spec
  '((help (single-char #\h) (value #f))
    (version (single-char #\v) (value #f))
    (work-dir (single-char #\w) (value #t))))

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

(define (installer-esp-volume-id installer partition)
  (let ((filename (installer-partition-filename installer partition)))
    (when (file-exists? filename)
      (command-output
       "/bin/sh" "-lc"
       (format #f "file ~a | awk -v 'RS=,' '/serial number/ { print $3 }'" filename)))))

(define (installer-data-filename installer)
  (format #f "~a/installer_data.json" (installer-output-dir installer)))

(define (installer-partition-filename installer partition)
  (format #f "~a/package/~a.img"
          (installer-work-dir installer)
          (sfdisk-partition-name partition)))

(define (efi-partition? partition)
  (equal? "C12A7328-F81F-11D2-BA4B-00A0C93EC93B"
          (sfdisk-partition-type partition)))

(define (other-partition? partition)
  (not (efi-partition? partition)))

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
     (copy-installer-data?? #t)
     (copy-firmware? #t)
     (image filename)
     (name (sfdisk-partition-name partition))
     (size (partition-size filename))
     (type "EFI")
     (volume-id (installer-esp-volume-id installer partition)))))

(define (build-other-partition installer table partition)
  (let ((filename (extract-partition installer table partition)))
    (format #t "  Partition #~a: ~a\n" (partition-index table partition) filename)
    (installer-partition
     (image filename)
     (name (sfdisk-partition-name partition))
     (size (partition-size filename))
     (type "Linux")
     (volume-id "TODO"))))

(define (build-partition installer table partition)
  (cond ((efi-partition? partition)
         (build-efi-partition installer table partition))
        ((other-partition? partition)
         (build-other-partition installer table partition))))

(define (build-partitions installer table)
  (map (lambda (partition)
         (build-partition installer table partition))
       (sfdisk-table-partitions table)))

(define* (build-os installer disk-image)
  (let ((table (sfdisk-list disk-image))
        (name (os-long-name installer disk-image)))
    (format #t "Building ~a ...\n" name)
    (installer-os
     (default-os-name (os-short-name disk-image))
     (icon "TODO")
     (name name)
     (package "TODO")
     (partitions (build-partitions installer table)))))

(define (build-installer-data installer)
  (installer-data
   (os-list (map (lambda (disk-image)
                   (build-os installer disk-image))
                 (installer-disk-images installer)))))

(define* (make-asahi-installer-package
          disk-images #:key
          (output-dir %output-dir)
          (package-version %package-version)
          (work-dir %work-dir))
  (format #t "Building Asahi Guix installer packages ...\n")
  (let* ((installer (installer
                     (disk-images disk-images)
                     (output-dir output-dir)
                     (package-version package-version)))
         (data (build-installer-data installer))
         (json-file (installer-data-filename installer))
         (json-doc (scm->json-string (installer-data->alist data))))
    (mkdir-p (dirname json-file))
    (call-with-output-file json-file
      (lambda (port)
        (set-port-encoding! port "UTF-8")
        (format port "~a\n" json-doc)))
    data))

(define (show-usage)
  (display "Usage: make-asahi-installer-package [options] DISK-IMAGE")
  (newline))

(define* (make-asahi-installer-package-main args)
  (let* ((options (getopt-long args option-spec))
         (work-dir (option-ref options 'work-dir %work-dir))
         (args (option-ref options '() #f)))
    (if (null? args)
        (show-usage)
        (make-asahi-installer-package (list (car args)) #:work-dir work-dir))))

;; (define my-data (make-asahi-installer-package (list "/gnu/store/hfr97d38hpgq2skh10192f1ik1smvrx7-asahi-base-image")))
