(define-module (asahi guix images sway)
  #:use-module (asahi guix systems sway)
  #:use-module (gnu system images asahi)
  #:export (asahi-sway-image))

(define asahi-sway-image
  (make-asahi-image 'asahi-sway-image asahi-sway-os))

asahi-sway-image
