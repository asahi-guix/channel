(define-module (asahi guix images edge)
  #:use-module (asahi guix images base)
  #:use-module (asahi guix systems base)
  #:export (asahi-edge-image))

(define asahi-edge-image
  (make-image asahi-edge-os 'asahi-edge-image))

asahi-edge-image
