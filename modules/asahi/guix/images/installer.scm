(define-module (asahi guix images installer)
  #:use-module (asahi guix systems base)
  #:use-module (gnu system images asahi)
  #:export (asahi-installer-image))

(define asahi-installer-image
  ;; TODO: Use installation os
  (make-asahi-image 'asahi-installer-image asahi-base-os))

asahi-installer-image
