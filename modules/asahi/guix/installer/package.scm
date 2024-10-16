(define-module (asahi guix installer package)
  #:use-module (asahi guix build sfdisk)
  #:use-module (asahi guix build utils)
  #:use-module (asahi guix installer data)
  #:use-module (asahi guix installer os)
  #:use-module (asahi guix installer partition)
  #:use-module (asahi guix search-paths)
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
            installer-package-artifact
            installer-package-build-dir
            installer-package-disk-image
            installer-package-icon
            installer-package-os-description
            installer-package-os-name
            installer-package-script
            installer-package?
            make-installer-package))

(define %build-dir "/tmp/asahi-guix/installer/work")

(define-record-type* <installer-package>
  installer-package
  make-installer-package
  installer-package?
  (artifact installer-package-artifact)
  (build-dir installer-package-build-dir (default %build-dir))
  (disk-image installer-package-disk-image)
  (icon installer-package-icon (default #f))
  (os-description installer-package-os-description)
  (os-name installer-package-os-name))

(define (parse-serial-number text)
  (let ((match (string-match "serial number\\s+(0x[0-9a-fA-F]+)" text)))
    (if (regexp-match? match)
        (match:substring match 1)
        #f)))

(define (installer-package-esp-dir package)
  (string-append (installer-package-build-dir package) "/package/esp"))

(define (installer-package-icon-path package)
  (string-append (installer-package-build-dir package) "/package/"
                 (basename (installer-package-icon package))))

(define (installer-package-archive-file package)
  (string-append (installer-package-artifact package) ".zip"))

(define (installer-package-archive-path package)
  (string-append (installer-package-build-dir package) "/os/"
                 (installer-package-archive-file package)))

(define (installer-package-esp-volume-id package partition)
  (let ((filename (installer-package-partition-path package partition)))
    (when (file-exists? filename)
      (parse-serial-number (command-output "file" filename)))))

(define (installer-package-metadata-file package)
  (string-append (installer-package-artifact package) ".json"))

(define (installer-package-metadata-path package)
  (string-append (installer-package-build-dir package) "/"
                 (installer-package-metadata-file package)))

(define (installer-package-partition-path package partition)
  (string-append (installer-package-build-dir package) "/package/"
                 (sfdisk-partition-name partition) ".img"))

(define (extract-partition package table partition)
  (let* ((filename (installer-package-partition-path package partition))
         (partition (sfdisk-extract-partition table partition filename)))
    (reset-timestamps filename)
    partition))

(define (partition-size filename)
  (format #f "~aB" (stat:size (stat filename))))

(define (unpack-efi-partition package partition)
  (let ((directory (installer-package-esp-dir package))
        (filename (installer-package-partition-path package partition)))
    (mkdir-p directory)
    (invoke "7z" "-aoa" "x" (format #f "-o~a" directory) filename)
    (reset-timestamps directory)
    directory))

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
  (let* ((archive-path (installer-package-archive-path package))
         (package-dir (format #f "~a/package" (installer-package-build-dir package))))
    (with-directory-excursion package-dir
      (mkdir-p (dirname archive-path))
      (reset-timestamps package-dir)
      ;; TODO: 7z compresses better but isn't reproducible?
      ;; (invoke "7z" "a" "-tzip" "-r" archive-path)
      (invoke "zip" "-9" "-r" "-X" archive-path ".")
      (reset-timestamps archive-path)
      archive-path)))

(define (build-icon package)
  (when (installer-package-icon package)
    (let ((filename (installer-package-icon-path package)))
      (mkdir-p (dirname filename))
      (copy-file (installer-package-icon package) filename)
      (reset-timestamps filename)
      filename)))

(define (build-os package)
  (let* ((disk-image (installer-package-disk-image package))
         (table (sfdisk-list disk-image))
         (partitions (build-partitions package table))
         (os (installer-os
              (default-os-name (installer-package-os-name package))
              (icon (basename (installer-package-icon package)))
              (name (installer-package-os-description package))
              (package (installer-package-archive-file package))
              (partitions partitions))))
    (build-icon package)
    (build-archive package)
    os))

(define (build-installer-data package)
  (let ((data (installer-data (os-list (list (build-os package)))))
        (filename (installer-package-metadata-path package)))
    (write-installer-data data filename)
    (reset-timestamps filename)
    data))

(define (print-package package)
  (format #t "  Artifact Name ......... ~a\n" (installer-package-artifact package))
  (format #t "  OS Name ............... ~a\n" (installer-package-os-name package))
  (format #t "  OS Description ........ ~a\n" (installer-package-os-description package))
  (format #t "  Build Directory ....... ~a\n" (installer-package-build-dir package))
  (format #t "  Disk Image ............ ~a\n" (installer-package-disk-image package))
  (format #t "  Icon .................. ~a\n" (installer-package-icon package)))

(define (build-package package)
  (format #t "Building Asahi installer package ...\n")
  (print-package package)
  (build-installer-data package))

;; Install

(define (install-os-path package directory)
  (string-append directory "/" %asahi-installer-os-path))

(define (install-archive package directory)
  (install-file (installer-package-archive-path package)
                (install-os-path package directory)))

(define (install-metadata package directory)
  (install-file (installer-package-metadata-path package)
                (install-os-path package directory)))

(define (install-package package directory)
  (format #t "Installing Asahi installer package ...\n")
  (print-package package)
  (format #t "  Install Directory ..... ~a\n" directory)
  (install-archive package directory)
  (install-metadata package directory))
