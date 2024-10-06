(define-module (asahi guix images sway)
  #:use-module (asahi guix systems sway)
  #:use-module (gnu system images asahi)
  #:export (asahi-sway-os-image))

(define asahi-sway-os-image
  (make-asahi-image 'asahi-guix-sway asahi-sway-os))

asahi-sway-os-image
