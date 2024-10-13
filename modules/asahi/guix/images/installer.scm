(define-module (asahi guix images installer)
  #:use-module (asahi guix systems base)
  #:use-module (gnu system images asahi installer)
  #:export (asahi-installer-os-image))

(define asahi-installer-os-image
  ;; TODO: Use installation os
  (make-installer-image 'asahi-guix-installer asahi-base-os))

asahi-installer-os-image
