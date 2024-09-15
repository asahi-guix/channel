(define-module (asahi guix images sway)
  #:use-module (asahi guix images base)
  #:use-module (asahi guix systems sway)
  #:export (asahi-sway-image))

(define asahi-sway-image
  (make-image asahi-sway-os 'asahi-sway-image))

asahi-sway-image
