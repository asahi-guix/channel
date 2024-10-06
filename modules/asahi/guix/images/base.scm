(define-module (asahi guix images base)
  #:use-module (asahi guix systems base)
  #:use-module (gnu system images asahi)
  #:export (asahi-base-os-image))

(define asahi-base-os-image
  (make-asahi-image 'asahi-guix-base asahi-base-os))

asahi-base-os-image
