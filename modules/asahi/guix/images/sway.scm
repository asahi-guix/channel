(define-module (asahi guix images sway)
  #:use-module (asahi guix systems sway)
  #:use-module (gnu system images asahi)
  #:export (asahi-guix-sway-image))

(define asahi-guix-sway-image
  (make-asahi-image 'asahi-guix-sway asahi-sway-os))

asahi-guix-sway-image
