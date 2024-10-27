(define-module (asahi guix build bootloader m1n1)
  #:use-module (gnu build image)
  #:use-module (guix build utils)
  #:export (install-m1n1-u-boot-grub
            m1n1-initialize-efi-partition))

(define* (write-m1n1 m1n1 target #:key dtbs u-boot)
  (mkdir-p (dirname target))
  (invoke "sh" "-c" (string-append "cat " m1n1 " > " target))
  (when (list? dtbs)
    (for-each (lambda (dtb)
                (invoke "sh" "-c" (string-append "cat " dtb " >> " target)))
              dtbs))
  (when (and u-boot (file-exists? u-boot))
    (invoke "sh" "-c" (string-append "gzip -c " u-boot " >> " target))))

(define* (update-m1n1 m1n1 target #:key dtbs u-boot)
  (let ((new-file (string-append target ".new"))
        (old-file (string-append target ".old")))
    (write-m1n1 m1n1 new-file #:dtbs dtbs #:u-boot u-boot)
    (when (file-exists? target)
      (rename-file target old-file))
    (rename-file new-file target)))

(define* (install-bootloader-to-esp bootloader esp)
  (format #t "Installing m1n1 to ~a ...\n" esp)
  (let* ((install-dir (string-append esp "/m1n1"))
         (target-bin (string-append install-dir "/boot.bin"))
         (dtbs (find-files (string-append bootloader "/lib/dtbs") "\\.*.dtb$"
                           #:stat stat #:directories? #t))
         (m1n1 (string-append bootloader "/libexec/m1n1.bin"))
         (u-boot (string-append bootloader "/libexec/u-boot-nodtb.bin")))
    (update-m1n1 m1n1 target-bin #:dtbs dtbs #:u-boot u-boot)))

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

(define (install-m1n1-u-boot bootloader efi-dir mount-point)
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
                           efi-dir)))
      (install-bootloader-to-esp bootloader target-esp))))

(define (install-m1n1-u-boot-grub bootloader efi-dir mount-point)
  (install-m1n1-u-boot bootloader efi-dir mount-point)
  (install-grub-efi-removable bootloader efi-dir mount-point))

(define* (m1n1-initialize-efi-partition
          root
          #:key bootloader-package grub-efi
          #:allow-other-keys)
  (initialize-efi-partition root #:grub-efi grub-efi)
  (install-bootloader-to-esp bootloader-package root))
