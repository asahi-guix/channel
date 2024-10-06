(define-module (asahi guix build installer package)
  #:use-module (asahi guix build installer operating-system)
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
            installer-data
            installer-data-os-list
            installer-data?
            installer-package
            installer-package-data-file
            installer-package-disk-image
            installer-package-icon
            installer-package-output-dir
            installer-package-work-dir
            installer-package-script
            installer?
            make-asahi-installer-package
            make-asahi-installer-package-main
            make-installer-data
            make-installer-package
            merge-installer-data
            write-installer-data
            read-installer-data))

(define %installer-data-file "installer_data.json")
(define %output-dir "/tmp/asahi-guix/installer/out")
(define %work-dir "/tmp/asahi-guix/installer/work")

(define %supported-firmwares
  (list "12.3" "12.3.1" "13.5"))

(define %os-names
  '(("asahi-guix-base" . "Asahi Guix Base")
    ("asahi-guix-edge" . "Asahi Guix Edge")
    ("asahi-guix-gnome" . "Asahi Guix with Gnome")
    ("asahi-guix-plasma" . "Asahi Guix with KDE Plasma")
    ("asahi-guix-sway" . "Asahi Guix with Sway")))

(define-record-type* <installer-package>
  installer-package
  make-installer-package
  installer-package?
  (data-file installer-package-data-file (default %installer-data-file))
  (disk-image installer-package-disk-image)
  (icon installer-package-icon (default #f))
  ;; (long-name installer-long-name)
  (output-dir installer-package-output-dir (default %output-dir))
  (version installer-package-version)
  (script installer-package-script (default #f))
  ;; (short-name installer-short-name)
  (work-dir installer-package-work-dir (default %work-dir)))

(define-record-type* <installer-data>
  installer-data
  make-installer-data
  installer-data?
  (os-list installer-data-os-list))

(define (installer-data->json data)
  (define os-list
    (map (lambda (os)
           (installer-os->json-alist os))
         (installer-data-os-list data)))
  `(("os_list" . ,(apply vector os-list))))

(define (json->installer-data alist)
  (installer-data
   (os-list
    (map json-alist->installer-os
         (vector->list (assoc-ref alist "os_list"))))))

(define (string-blank? s)
  (string-match "^\\s*$" s))

(define (disk-image-name filename)
  (let ((parts (string-split filename #\/)))
    (string-join (cdr (string-split (last parts) #\-)) "-")))

(define (os-short-name disk-image)
  (or (assoc-ref %os-names (disk-image-name disk-image)) "Asahi Guix"))

(define (os-long-name installer disk-image)
  (string-append (os-short-name disk-image)))

(define (installer-esp-dir installer)
  (format #f "~a/package/esp" (installer-package-work-dir installer)))

(define (installer-package-icon-filename installer disk-image)
  (format #f "~a.icns" (disk-image-name disk-image)))

(define (installer-package-icon-workdir-path installer disk-image)
  (string-append (installer-package-work-dir installer) "/package/"
                 (installer-package-icon-filename installer disk-image)))

(define (installer-package-filename installer disk-image)
  (format #f "~a.zip" (disk-image-name disk-image)))

(define (installer-package-output-path installer disk-image)
  (format #f "~a/~a" (installer-package-output-dir installer)
          (installer-package-filename installer disk-image)))

(define (parse-serial-number text)
  (let ((match (string-match "serial number\\s+(0x[0-9a-fA-F]+)" text)))
    (if (regexp-match? match)
        (match:substring match 1)
        #f)))

(define (installer-esp-volume-id installer partition)
  (let ((filename (installer-partition-filename installer partition)))
    (when (file-exists? filename)
      (parse-serial-number (command-output "file" filename)))))

(define (installer-data-output-path installer)
  (format #f "~a/~a" (installer-package-output-dir installer)
          (installer-package-data-file installer)))

(define (installer-script-output-file installer)
  (string-replace-substring (installer-package-data-file installer) ".json" ".sh"))

(define (installer-script-output-path installer)
  (string-append (installer-package-output-dir installer) "/"
                 (installer-script-output-file installer)))

(define (installer-partition-filename installer partition)
  (format #f "~a/package/~a.img"
          (installer-package-work-dir installer)
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
  (format #f "~a/~a" (installer-package-work-dir installer)
          (list-index (lambda (p)
                        (equal? partition p))
                      (sfdisk-table-partitions table))))

(define (extract-command installer table partition)
  (let ((work-dir (installer-package-work-dir installer)))
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
  (let* ((filename (extract-partition installer table partition))
         (work-dir (installer-package-work-dir installer))
         (size (partition-size filename))
         (volume-id (installer-esp-volume-id installer partition)))
    (format #t "  Partition #~a: ~a\n" (partition-index table partition) filename)
    (unpack-efi-partition installer partition)
    (delete-file filename)
    (installer-partition
     (copy-firmware? #t)
     (copy-installer-data? #t)
     (format "fat")
     (name (sfdisk-partition-name partition))
     (size size)
     (source "esp")
     (type "EFI")
     (volume-id volume-id))))

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

(define (installer-build-archive installer disk-image)
  (let* ((archive-name (installer-package-output-path installer disk-image))
         (package-dir (format #f "~a/package" (installer-package-work-dir installer))))
    (with-directory-excursion package-dir
      (invoke "7z" "a" "-tzip" "-r" archive-name))))

(define (installer-build-icon installer disk-image)
  (when (installer-package-icon installer)
    (let ((filename (installer-package-icon-workdir-path installer disk-image)))
      (mkdir-p (dirname filename))
      (copy-file (installer-package-icon installer) filename))))

(define* (build-os installer)
  (let* ((disk-image (installer-package-disk-image installer))
         (table (sfdisk-list disk-image))
         (name (os-long-name installer disk-image)))
    (format #t "Building ~a ...\n" name)
    (let ((os (installer-os
               (default-os-name (os-short-name disk-image))
               (icon (installer-package-icon-filename installer disk-image))
               (name name)
               (package (installer-package-filename installer disk-image))
               (partitions (build-partitions installer table)))))
      (installer-build-icon installer disk-image)
      (installer-build-archive installer disk-image)
      os)))

(define (build-installer-data installer)
  (installer-data
   (os-list (list (build-os installer)))))

(define (merge-installer-data data-1 data-2)
  (installer-data
   (os-list (append (installer-data-os-list data-1)
                    (installer-data-os-list data-2)))))

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
  (let ((filename (installer-data-output-path installer)))
    (write-installer-data data filename)))

(define (save-installer-script installer)
  (let ((source (installer-package-script installer))
        (target (installer-script-output-path installer)))
    (mkdir-p (dirname target))
    (copy-file source target)
    (substitute* target
      (("INSTALLER_DATA=.*")
       (string-append "INSTALLER_DATA="
                      (installer-package-data-file installer) "\n"))
      (("INSTALLER_DATA_ALT=.*")
       (string-append "INSTALLER_DATA_ALT="
                      (installer-package-data-file installer) "\n")))
    target))

(define (print-package-info package)
  (format #t "  Data File ............. ~a\n" (installer-package-data-file package))
  (format #t "  Icon .................. ~a\n" (installer-package-icon package))
  (format #t "  Output Directory ...... ~a\n" (installer-package-output-dir package))
  (format #t "  Version ............... ~a\n" (installer-package-version package))
  (format #t "  Work Directory ........ ~a\n" (installer-package-work-dir package)))

(define (build-installer-package package)
  (format #t "Building Asahi Guix installer packages ...\n")
  (print-package-info package)
  (let ((data (build-installer-data package)))
    (save-installer-data package data)
    (save-installer-script package)
    data))
