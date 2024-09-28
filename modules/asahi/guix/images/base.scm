(define-module (asahi guix images base)
  #:use-module (asahi guix systems base)
  #:use-module (gnu system images asahi)
  #:export (asahi-guix-base-image))

(define asahi-guix-base-image
  (make-asahi-image 'asahi-guix-base asahi-base-os))

asahi-guix-base-image
