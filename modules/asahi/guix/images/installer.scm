(define-module (asahi guix images installer)
  #:use-module (asahi guix systems base)
  #:use-module (gnu system images asahi)
  #:export (asahi-guix-installer-image))

(define asahi-guix-installer-image
  ;; TODO: Use installation os
  (make-asahi-image 'asahi-guix-installer asahi-base-os))

asahi-guix-installer-image
