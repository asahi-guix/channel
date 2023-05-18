(define-module (asahi guix build bootloader m1n1)
  #:export (install-m1n1-u-boot-grub)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (ice-9 binary-ports))

(define (copy-bytes from to)
  (let ((result (get-u8 from)))
    (when (not (eof-object? result))
      (put-u8 to result)
      (copy-bytes from to))))

(define (append-file file out)
  (call-with-input-file file
    (lambda (in) (copy-bytes in out))
    #:binary #t))

(define (append-file-gzip file out)
  (call-with-compressed-output-port 'gzip out
    (lambda (out) (append-file file out))))

(define* (write-m1n1 m1n1 target #:key dtbs u-boot)
  (mkdir-p (dirname target))
  (call-with-output-file target
    (lambda (out)
      (append-file m1n1 out)
      (for-each (lambda (dtb) (append-file dtb out)) dtbs)
      (when (file-exists? u-boot)
        (append-file-gzip u-boot out)))
    #:binary #t))

(define* (update-m1n1 m1n1 target #:key dtbs u-boot)
  (let ((new-file (string-append target ".new"))
        (old-file (string-append target ".old")))
    (write-m1n1 m1n1 new-file #:dtbs dtbs #:u-boot u-boot)
    (when (file-exists? target)
      (rename-file target old-file))
    (rename-file new-file target)))

;; TODO: Howto re-use this function from the grub bootloader module?
(define (install-grub-efi-removable bootloader efi-dir mount-point)
  ;; NOTE: mount-point is /mnt in guix system init /etc/config.scm /mnt/point
  ;; NOTE: efi-dir comes from target list of booloader configuration
  ;; There is nothing useful to do when called in the context of a disk
  ;; image generation.
  (when efi-dir
    ;; Install GRUB onto the EFI partition mounted at EFI-DIR, for the
    ;; system whose root is mounted at MOUNT-POINT.
    (let ((grub-install (string-append bootloader "/sbin/grub-install"))
          (install-dir (string-append mount-point "/boot"))
          ;; When installing Guix, it's common to mount EFI-DIR below
          ;; MOUNT-POINT rather than /boot/efi on the live image.
          (target-esp (if (file-exists? (string-append mount-point efi-dir))
                          (string-append mount-point efi-dir)
                          efi-dir)))
      ;; Tell 'grub-install' that there might be a LUKS-encrypted /boot or
      ;; root partition.
      (setenv "GRUB_ENABLE_CRYPTODISK" "y")
      (invoke/quiet grub-install "--boot-directory" install-dir
                    "--removable"
                    ;; "--no-nvram"
                    "--bootloader-id=Guix"
                    "--efi-directory" target-esp))))

(define (install-m1n1-u-boot-grub bootloader efi-dir mount-point)
  ;; NOTE: mount-point is /mnt in guix system init /etc/config.scm /mnt/point
  ;; NOTE: efi-dir comes from target list of booloader configuration
  ;; There is nothing useful to do when called in the context of a disk
  ;; image generation.
  (when efi-dir
    ;; Install M1N1 onto the EFI partition mounted at EFI-DIR, for the
    ;; system whose root is mounted at MOUNT-POINT.
    (let* ( ;; When installing Guix, it's common to mount EFI-DIR below
           ;; MOUNT-POINT rather than /boot/efi on the live image.
           (target-esp (if (file-exists? (string-append mount-point efi-dir))
                           (string-append mount-point efi-dir)
                           efi-dir))
           (install-dir (string-append target-esp "/m1n1"))
           (target-bin (string-append install-dir "/boot.bin"))
           (dtbs (find-files (string-append bootloader "/lib/dtbs") "\\.*.dtb$"
                             #:stat stat #:directories? #t))
           (m1n1 (string-append bootloader "/libexec/m1n1.bin"))
           (u-boot (string-append bootloader "/libexec/u-boot-nodtb.bin")))
      (update-m1n1 m1n1 target-bin #:dtbs dtbs #:u-boot u-boot)))
  ;; Install Grub
  (install-grub-efi-removable bootloader efi-dir mount-point))
