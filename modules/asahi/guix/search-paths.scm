(define-module (asahi guix search-paths)
  #:use-module (guix search-paths)
  #:export (%asahi-installer-os-path
            $ASAHI_INSTALLER_OS_PATH))

(define %asahi-installer-os-path
  "share/asahi-installer/os")

(define $ASAHI_INSTALLER_OS_PATH
  (search-path-specification
   (variable "ASAHI_INSTALLER_OS_PATH")
   (files (list %asahi-installer-os-path))))
