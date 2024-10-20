(define-module (asahi guix images sway)
  #:use-module (asahi guix systems sway)
  #:use-module (gnu system images asahi installer)
  #:export (asahi-sway-os-image))

(define asahi-sway-os-image
  (make-installer-image 'asahi-sway-image asahi-sway-os))

asahi-sway-os-image
