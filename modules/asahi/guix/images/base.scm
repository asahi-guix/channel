(define-module (asahi guix images base)
  #:use-module (asahi guix systems base)
  #:use-module (gnu system images asahi installer)
  #:export (asahi-base-os-image))

(define asahi-base-os-image
  (make-installer-image 'asahi-guix-base asahi-base-os))

asahi-base-os-image
