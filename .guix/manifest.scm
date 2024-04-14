(use-modules (asahi guix initrd)
             (asahi guix packages linux)
             (asahi guix system base)
             (gnu packages base)
             (gnu system)
             (guix gexp)
             (guix packages)
             (guix profiles))

(define* (package->manifest-entry* package system
                                   #:key target)
  "Return a manifest entry for PACKAGE on SYSTEM, optionally cross-compiled to
TARGET."
  (manifest-entry
    (inherit (package->manifest-entry package))
    (name (string-append (package-name package) "." system
                         (if target
                             (string-append "." target)
                             "")))
    (item (with-parameters ((%current-system system)
                            (%current-target-system target))
            package))))

(define native-builds
  (manifest
   (append (map (lambda (system)
                  (package->manifest-entry* hello system))
                '("aarch64-linux")))))

(concatenate-manifests
 (list (manifest
        (list (manifest-entry
                (name "asahi-guix-system")
                (version "1")
                (item (asahi-operating-system
                       #:esp-uuid "41F0-16FF"
                       #:initrd-modules asahi-initrd-modules-edge
                       #:kernel asahi-linux-edge)))))))


;; (concatenate-manifests (list native-builds))
