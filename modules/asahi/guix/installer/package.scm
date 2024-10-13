(define-module (asahi guix installer package)
  #:use-module (asahi guix build sfdisk)
  #:use-module (asahi guix build utils)
  #:use-module (asahi guix installer data)
  #:use-module (asahi guix installer os)
  #:use-module (asahi guix installer partition)
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
  #:export (build-package
            install-package
            installer-package
            installer-package-artifact-name
            installer-package-default-os-name
            installer-package-disk-image
            installer-package-icon
            installer-package-long-name
            installer-package-script
            installer-package-build-dir
            installer-package-version
            installer-package?
            make-installer-package))

(define %build-dir "/tmp/asahi-guix/installer/work")

(define-record-type* <installer-package>
  installer-package
  make-installer-package
  installer-package?
  (artifact-name installer-package-artifact-name)
  (build-dir installer-package-build-dir (default %build-dir))
  (default-os-name installer-package-default-os-name)
  (disk-image installer-package-disk-image)
  (icon installer-package-icon (default #f))
  (long-name installer-package-long-name)
  (script installer-package-script (default #f))
  (version installer-package-version (default #f) ))

(define (parse-serial-number text)
  (let ((match (string-match "serial number\\s+(0x[0-9a-fA-F]+)" text)))
    (if (regexp-match? match)
        (match:substring match 1)
        #f)))

(define (installer-package-esp-dir package)
  (format #f "~a/package/esp" (installer-package-build-dir package)))

(define (installer-package-icon-path package)
  (string-append (installer-package-build-dir package) "/package/"
                 (basename (installer-package-icon package))))

(define (installer-package-archive-file package)
  (format #f "~a.zip" (installer-package-artifact-name package)))

(define (installer-package-archive-path package)
  (format #f "~a/~a" (installer-package-build-dir package)
          (installer-package-archive-file package)))

(define (installer-package-esp-volume-id package partition)
  (let ((filename (installer-package-partition-path package partition)))
    (when (file-exists? filename)
      (parse-serial-number (command-output "file" filename)))))

(define (installer-package-metadata-file package)
  (string-append (installer-package-artifact-name package) ".json"))

(define (installer-package-metadata-path package)
  (string-append (installer-package-build-dir package) "/"
                 (installer-package-metadata-file package)))

(define (installer-package-script-path package)
  (string-append (installer-package-build-dir package) "/"
                 (installer-package-artifact-name package) ".sh"))

(define (installer-package-partition-path package partition)
  (format #f "~a/package/~a.img"
          (installer-package-build-dir package)
          (sfdisk-partition-name partition)))

(define (extract-partition package table partition)
  (let ((filename (installer-package-partition-path package partition)))
    (sfdisk-extract-partition table partition filename)))

(define (partition-size filename)
  (format #f "~aB" (stat:size (stat filename))))

(define (unpack-efi-partition package partition)
  (let ((directory (installer-package-esp-dir package))
        (filename (installer-package-partition-path package partition)))
    (mkdir-p directory)
    (invoke "7z" "-aoa" "x" (format #f "-o~a" directory) filename)))

(define (build-efi-partition package table partition)
  (let* ((filename (extract-partition package table partition))
         (build-dir (installer-package-build-dir package))
         (size (partition-size filename))
         (volume-id (installer-package-esp-volume-id package partition)))
    (format #t "  Partition #~a: ~a\n" (sfdisk-partition-index table partition) filename)
    (unpack-efi-partition package partition)
    (delete-file filename)
    (installer-partition
     (copy-firmware? #t)
     (copy-installer-metadata? #t)
     (format "fat")
     (name (sfdisk-partition-name partition))
     (size size)
     (source "esp")
     (type "EFI")
     (volume-id volume-id))))

(define (build-linux-partition package table partition)
  (let ((filename (extract-partition package table partition)))
    (format #t "  Partition #~a: ~a\n" (sfdisk-partition-index table partition) filename)
    (installer-partition
     (expand? #t)
     (image (basename filename))
     (name (sfdisk-partition-name partition))
     (size (partition-size filename))
     (type "Linux"))))

(define (build-partition package table partition)
  (cond ((sfdisk-partition-efi? partition)
         (build-efi-partition package table partition))
        ((sfdisk-partition-linux? partition)
         (build-linux-partition package table partition))))

(define (build-partitions package table)
  (map (lambda (partition)
         (build-partition package table partition))
       (sfdisk-table-partitions table)))

(define (build-archive package)
  (let* ((archive-name (installer-package-archive-path package))
         (package-dir (format #f "~a/package" (installer-package-build-dir package))))
    (with-directory-excursion package-dir
      (reset-timestamps package-dir)
      (invoke "7z" "a" "-tzip" "-r" archive-name))))

(define (build-icon package)
  (when (installer-package-icon package)
    (let ((filename (installer-package-icon-path package)))
      (mkdir-p (dirname filename))
      (copy-file (installer-package-icon package) filename))))

(define (build-os package)
  (let* ((disk-image (installer-package-disk-image package))
         (table (sfdisk-list disk-image))
         (partitions (build-partitions package table))
         (os (installer-os
              (default-os-name (installer-package-default-os-name package))
              (icon (basename (installer-package-icon package)))
              (name (installer-package-long-name package))
              (package (installer-package-archive-file package))
              (partitions partitions))))
    (build-icon package)
    (build-archive package)
    os))

(define (build-installer-data package)
  (let ((data (installer-data (os-list (list (build-os package)))))
        (filename (installer-package-metadata-path package)))
    (write-installer-data data filename)
    data))

(define (build-installer-script package)
  (let ((source (installer-package-script package))
        (target (installer-package-script-path package)))
    (mkdir-p (dirname target))
    (copy-file source target)
    (substitute* target
      (("INSTALLER_DATA=.*")
       (string-append "INSTALLER_DATA="
                      (installer-package-metadata-file package) "\n"))
      (("INSTALLER_DATA_ALT=.*")
       (string-append "INSTALLER_DATA_ALT="
                      (installer-package-metadata-file package) "\n")))
    target))

(define (print-package package)
  (format #t "  Artifact Name ......... ~a\n" (installer-package-artifact-name package))
  (format #t "  Build Directory ....... ~a\n" (installer-package-build-dir package))
  (format #t "  Default OS Name ....... ~a\n" (installer-package-default-os-name package))
  (format #t "  Disk Image ............ ~a\n" (installer-package-disk-image package))
  (format #t "  Icon .................. ~a\n" (installer-package-icon package))
  (format #t "  Long Name ............. ~a\n" (installer-package-long-name package))
  (format #t "  Script ................ ~a\n" (installer-package-script package))
  (format #t "  Version ............... ~a\n" (installer-package-version package)))

(define (build-package package)
  (format #t "Building Asahi installer package ...\n")
  (print-package package)
  (let ((data (build-installer-data package)))
    (build-installer-script package)
    data))

(define (install-package package directory)
  (format #t "Installing Asahi installer package ...\n")
  (print-package package)
  (format #t "  Install Directory ..... ~a\n" directory)
  (let ((os-dir (string-append directory "/share/asahi-installer/os")))
    (mkdir-p os-dir)
    (install-file (installer-package-archive-path package) os-dir)
    (install-file (installer-package-metadata-path package) os-dir)
    (install-file (installer-package-script-path package) os-dir)))
