(define-module (asahi guix build installer package)
  #:use-module (asahi guix build installer metadata)
  #:use-module (asahi guix build installer os)
  #:use-module (asahi guix build installer partition)
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
  #:export (build-installer-package
            installer-package
            installer-package-artifact-name
            installer-package-data-file
            installer-package-disk-image
            installer-package-icon
            installer-package-long-name
            installer-package-output-dir
            installer-package-script
            installer-package-default-os-name
            installer-package-work-dir
            installer-package?
            make-asahi-installer-package
            make-asahi-installer-package-main
            make-installer-package))

(define %installer-metadata-file "installer_data.json")
(define %output-dir "/tmp/asahi-guix/installer/out")
(define %work-dir "/tmp/asahi-guix/installer/work")

(define-record-type* <installer-package>
  installer-package
  make-installer-package
  installer-package?
  (artifact-name installer-package-artifact-name)
  (data-file installer-package-data-file (default %installer-metadata-file))
  (disk-image installer-package-disk-image)
  (icon installer-package-icon (default #f))
  (long-name installer-package-long-name)
  (output-dir installer-package-output-dir (default %output-dir))
  (script installer-package-script (default #f))
  (default-os-name installer-package-default-os-name)
  (version installer-package-version)
  (work-dir installer-package-work-dir (default %work-dir)))

(define (disk-image-name filename)
  (let ((parts (string-split filename #\/)))
    (string-join (cdr (string-split (last parts) #\-)) "-")))

(define (installer-esp-dir package)
  (format #f "~a/package/esp" (installer-package-work-dir package)))

(define (installer-package-icon-filename package)
  (format #f "~a.icns" (installer-package-artifact-name package)))

(define (installer-package-icon-workdir-path package)
  (string-append (installer-package-work-dir package) "/package/"
                 (installer-package-icon-filename package)))

(define (installer-package-filename package)
  (format #f "~a.zip" (installer-package-artifact-name package)))

(define (installer-package-output-path package)
  (format #f "~a/~a" (installer-package-output-dir package)
          (installer-package-filename package)))

(define (parse-serial-number text)
  (let ((match (string-match "serial number\\s+(0x[0-9a-fA-F]+)" text)))
    (if (regexp-match? match)
        (match:substring match 1)
        #f)))

(define (installer-esp-volume-id package partition)
  (let ((filename (installer-partition-filename package partition)))
    (when (file-exists? filename)
      (parse-serial-number (command-output "file" filename)))))

(define (installer-package-metadata-filename package)
  (string-append (installer-package-artifact-name package) ".json"))

(define (installer-metadata-output-path package)
  (string-append (installer-package-output-dir package) "/"
                 (installer-package-artifact-name package) ".json"))

(define (installer-script-output-path package)
  (string-append (installer-package-output-dir package) "/"
                 (installer-package-artifact-name package) ".sh"))

(define (installer-partition-filename package partition)
  (format #f "~a/package/~a.img"
          (installer-package-work-dir package)
          (sfdisk-partition-name partition)))

(define (partition-index table partition)
  (list-index (lambda (p)
                (equal? partition p))
              (sfdisk-table-partitions table)))

(define (partition-filename package table partition)
  (format #f "~a/~a" (installer-package-work-dir package)
          (list-index (lambda (p)
                        (equal? partition p))
                      (sfdisk-table-partitions table))))

(define (extract-command package table partition)
  (let ((work-dir (installer-package-work-dir package)))
    (list "dd"
          (format #f "if=~a" (sfdisk-table-device table))
          (format #f "of=~a" (installer-partition-filename package partition))
          (format #f "skip=~a" (sfdisk-partition-start partition))
          (format #f "count=~a" (sfdisk-partition-size partition))
          (format #f "bs=~a" (sfdisk-table-sector-size table)))))

(define (extract-partition package table partition)
  (let ((command (extract-command package table partition))
        (filename (installer-partition-filename package partition)))
    (mkdir-p (dirname filename))
    (apply system* command)
    filename))

(define (partition-size filename)
  (format #f "~aB" (stat:size (stat filename))))

(define (unpack-efi-partition package partition)
  (let ((directory (installer-esp-dir package))
        (filename (installer-partition-filename package partition)))
    (mkdir-p directory)
    (invoke "7z" "-aoa" "x" (format #f "-o~a" directory) filename)))

(define (build-efi-partition package table partition)
  (let* ((filename (extract-partition package table partition))
         (work-dir (installer-package-work-dir package))
         (size (partition-size filename))
         (volume-id (installer-esp-volume-id package partition)))
    (format #t "  Partition #~a: ~a\n" (partition-index table partition) filename)
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
    (format #t "  Partition #~a: ~a\n" (partition-index table partition) filename)
    (installer-partition
     (expand? #t)
     (image (basename filename))
     (name (sfdisk-partition-name partition))
     (size (partition-size filename))
     (type "Linux"))))

(define (build-partition package table partition)
  (cond ((sfdisk-efi-partition? partition)
         (build-efi-partition package table partition))
        ((sfdisk-linux-partition? partition)
         (build-linux-partition package table partition))))

(define (build-partitions package table)
  (map (lambda (partition)
         (build-partition package table partition))
       (sfdisk-table-partitions table)))

(define (build-archive package)
  (let* ((archive-name (installer-package-output-path package))
         (package-dir (format #f "~a/package" (installer-package-work-dir package))))
    (with-directory-excursion package-dir
      (invoke "7z" "a" "-tzip" "-r" archive-name))))

(define (build-icon package)
  (when (installer-package-icon package)
    (let ((filename (installer-package-icon-workdir-path package)))
      (mkdir-p (dirname filename))
      (copy-file (installer-package-icon package) filename))))

(define (build-os package)
  (let* ((disk-image (installer-package-disk-image package))
         (table (sfdisk-list disk-image))
         (partitions (build-partitions package table)))
    (format #t "Building ~a ...\n" (installer-package-default-os-name package))
    (let ((os (installer-os
               (default-os-name (installer-package-default-os-name package))
               (icon (installer-package-icon-filename package))
               (name (installer-package-long-name package))
               (package (installer-package-filename package))
               (partitions partitions))))
      (build-icon package)
      (build-archive package)
      os)))

(define (build-installer-metadata package)
  (installer-metadata (os-list (list (build-os package)))))

(define (save-installer-metadata package data)
  (let ((filename (installer-metadata-output-path package)))
    (write-installer-metadata data filename)))

(define (save-installer-script package)
  (let ((source (installer-package-script package))
        (target (installer-script-output-path package)))
    (mkdir-p (dirname target))
    (copy-file source target)
    (substitute* target
      (("INSTALLER_DATA=.*")
       (string-append "INSTALLER_DATA="
                      (installer-package-metadata-filename package) "\n"))
      (("INSTALLER_DATA_ALT=.*")
       (string-append "INSTALLER_DATA_ALT="
                      (installer-package-metadata-filename package) "\n")))
    target))

(define (print-installer-package package)
  (format #t "  Data File ............. ~a\n" (installer-package-data-file package))
  (format #t "  Icon .................. ~a\n" (installer-package-icon package))
  (format #t "  Output Directory ...... ~a\n" (installer-package-output-dir package))
  (format #t "  Version ............... ~a\n" (installer-package-version package))
  (format #t "  Work Directory ........ ~a\n" (installer-package-work-dir package)))

(define (build-installer-package package)
  (format #t "Building Asahi Guix installer packages ...\n")
  (print-installer-package package)
  (let ((data (build-installer-metadata package)))
    (save-installer-metadata package data)
    (save-installer-script package)
    data))
