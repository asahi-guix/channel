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

(define (os-long-name installer disk-image)
  (string-replace-substring
   (os-short-name disk-image)
   "Asahi Guix"
   (format #f "Asahi Guix ~a" (installer-package-version installer))))

(define (efi-partition? partition)
  (equal? "C12A7328-F81F-11D2-BA4B-00A0C93EC93B"
          (sfdisk-partition-type partition)))

(define (other-partition? partition)
  (not (efi-partition? partition)))

(define (partition-filename installer table partition)
  (format #f "~a/~a" (installer-work-dir installer)
          (list-index (lambda (p)
                        (equal? partition p))
                      (sfdisk-table-partitions table))))

(define (extract-command installer table partition)
  (let ((work-dir (installer-work-dir installer)))
    (list "dd"
          (format #f "if=~a" (sfdisk-table-device table))
          (format #f "of=~a" (partition-filename installer table partition))
          (format #f "skip=~a" (sfdisk-partition-start partition))
          (format #f "count=~a" (sfdisk-partition-size partition))
          (format #f "bs=~a" (sfdisk-table-sector-size table)))))

(define (extract-partition installer table partition)
  (let ((command (extract-command installer table partition))
        (filename (partition-filename installer table partition)))
    (mkdir-p (dirname filename))
    (apply system* command)
    filename))

(define (partition-size filename)
  (format #f "~aB" (stat:size (stat filename))))

(define (build-efi-partition installer table partition)
  (let ((filename (extract-partition installer table partition)))
    (installer-partition
     (copy-firmware-installer-data? #t)
     (copy-firmware? #t)
     (image filename)
     (name (sfdisk-partition-name partition))
     (size (partition-size filename))
     (type "EFI")
     (volume-id "TODO"))))

(define (build-other-partition installer table partition)
  (let ((filename (extract-partition installer table partition)))
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
  (build-installer-data
   (installer
    (disk-images disk-images)
    (output-dir output-dir)
    (package-version package-version))))

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
