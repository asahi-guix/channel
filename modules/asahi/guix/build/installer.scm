(define-module (asahi guix build installer)
  #:use-module (asahi guix build sfdisk)
  #:use-module (asahi guix build utils)
  #:use-module (gnu packages package-management)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:export (make-asahi-installer-package
            make-asahi-installer-package-main))

(define %supported-firmwares
  (list "12.3" "12.3.1" "13.5"))

(define %work-dir
  "/tmp/asahi-guix/installer")

(define %os-names
  '(("asahi-base-image" . "Asahi Guix Base")
    ("asahi-edge-image" . "Asahi Guix Edge")
    ("asahi-gnome-image" . "Asahi Guix with Gnome")
    ("asahi-plasma-image" . "Asahi Guix with KDE Plasma")
    ("asahi-sway-image" . "Asahi Guix with Sway")))

(define-record-type* <asahi-installer-package-configuration>
  asahi-installer-package-configuration
  make-asahi-installer-package-configuration
  asahi-installer-package-configuration?
  (source-image asahi-installer-package-configuration-source-image))

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
  (copy-firmware-installer-data? installer-partition-copy-firmware-installer-data? (default #f))
  (copy-firmware? installer-partition-copy-firmware? (default #f))
  (expand? installer-partition-expand? (default #f))
  (format installer-partition-format (default #f))
  (image installer-partition-image)
  (name installer-partition-name)
  (size installer-partition-size)
  (source installer-partition-source (default #f))
  (type installer-partition-type)
  (volume-id installer-partition-volume-id))

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

(define (os-long-name disk-image)
  (string-replace-substring
   (os-short-name disk-image)
   "Asahi Guix"
   (format #f "Asahi Guix ~a" (package-version guix))))

(define (efi-partition? partition)
  (equal? "C12A7328-F81F-11D2-BA4B-00A0C93EC93B"
          (sfdisk-partition-type partition)))

(define (other-partition? partition)
  (not (efi-partition? partition)))

(define (partition-filename table partition work-dir)
  (format #f "~a/~a" work-dir
          (list-index (lambda (p)
                        (equal? partition p))
                      (sfdisk-table-partitions table))))

(define (extract-command table partition work-dir)
  (list "dd"
        (format #f "if=~a" (sfdisk-table-device table))
        (format #f "of=~a" (partition-filename table partition work-dir))
        (format #f "skip=~a" (sfdisk-partition-start partition))
        (format #f "count=~a" (sfdisk-partition-size partition))
        (format #f "bs=~a" (sfdisk-table-sector-size table))))

(define (extract-partition table partition work-dir)
  (let ((command (extract-command table partition work-dir))
        (filename (partition-filename table partition work-dir)))
    (mkdir-p (dirname filename))
    (apply system* command)
    filename))

(define (partition-size filename)
  (format #f "~aB" (stat:size (stat filename))))

(define (build-efi-partition table partition work-dir)
  (let ((filename (extract-partition table partition work-dir)))
    (installer-partition
     (copy-firmware-installer-data? #t)
     (copy-firmware? #t)
     (image filename)
     (name (sfdisk-partition-name partition))
     (size (partition-size filename))
     (type "EFI")
     (volume-id "TODO"))))

(define (build-other-partition table partition work-dir)
  (let ((filename (extract-partition table partition work-dir)))
    (installer-partition
     (image filename)
     (name (sfdisk-partition-name partition))
     (size (partition-size filename))
     (type "Linux")
     (volume-id "TODO"))))

(define (build-partition table partition work-dir)
  (cond ((efi-partition? partition)
         (build-efi-partition table partition work-dir))
        ((other-partition? partition)
         (build-other-partition table partition work-dir))))

(define (build-partitions table work-dir)
  (map (lambda (partition)
         (build-partition table partition work-dir))
       (sfdisk-table-partitions table)))

(define* (build-os disk-image #:key (work-dir %work-dir))
  (let ((table (sfdisk-list disk-image)))
    (installer-os
     (default-os-name (os-short-name disk-image))
     (icon "TODO")
     (name (os-long-name disk-image))
     (package "TODO")
     (partitions (build-partitions table work-dir)))))

(define* (make-asahi-installer-package disk-images #:key (work-dir %work-dir))
  (display "YO\n")
  ;; (installer-data
  ;;  (os-list (map (lambda (disk-image)
  ;;                  (build-os disk-image #:work-dir work-dir))
  ;;                disk-images)))
  )

(define (show-usage)
  (display "Usage: make-asahi-installer-package [options] DISK-IMAGE")
  (newline))

(define* (make-asahi-installer-package-main args)
  (let* ((options (getopt-long args option-spec))
         (work-dir (option-ref options 'work-dir %work-dir))
         (args (option-ref options '() #f)))
    (if (null? args)
        (show-usage)
        (make-asahi-installer-package (car args) #:work-dir work-dir))))

;; (make-asahi-installer-package (list "/gnu/store/hfr97d38hpgq2skh10192f1ik1smvrx7-asahi-base-image"))
